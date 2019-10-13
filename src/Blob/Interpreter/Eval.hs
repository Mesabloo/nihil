-- The Blob programming language's interpreter.
-- Copyright (c) 2019 Mesabloo

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE LambdaCase, TupleSections #-}

-- | This module holds all the interpreting functions.
module Blob.Interpreter.Eval where

import Blob.Language.Desugaring.Types (Expr(..), Literal(..), Pattern(..), Statement(..))
import qualified Data.Map as Map
import Blob.Interpreter.Types
import Control.Monad.Reader
import Control.Monad.Except
import Control.Applicative
import Text.PrettyPrint.Leijen hiding ((<$>), empty)
import Data.Maybe
import Data.List.Extra (snoc)
import Blob.Language.Parsing.Annotation
import Control.Lens hiding (snoc)

-- | Evaluates an expression.
evaluate :: Annotated Expr -> EvalEnv Value
evaluate (ELit (LInt v) :- _)    = pure $ VInt v
evaluate (ELit (LDec v) :- _)    = pure $ VDec v
evaluate (ELit (LChr v) :- _)    = pure $ VChr v
evaluate (EId id' :- _)          = do
    look <- vals `views` Map.lookup id'

    pure $ fromMaybe (VCon id' []) look
evaluate (ETuple es :- _)        = VTuple <$> mapM evaluate es
evaluate (ELam x e :- _)         = VLam x e <$> view vals
    -- Do not unwrap the lambda, as an argument has not been given yet
evaluate (ELet ss e :- _)    = do
    env' <- (catMaybes <$>) . forM ss $ \case
        Definition name expr :- _ -> Just . (name,) <$> evaluate expr
        _ -> pure Nothing
    local (vals %~ (Map.fromList env' <>)) $
        evaluate e
evaluate (EApp f x :- _)         = do
    x' <- evaluate x
    f' <- evaluate f
    case f' of
        VLam x e c -> do
            vals' <- unpackPattern x' x <|> makeMatchError
            local (vals %~ ((vals' <> c) <>)) $
                evaluate e
        HLam f''   -> f'' x'
        VCon id' e -> pure $ VCon id' (snoc e x')
        v          -> throwError . text $ "Developer error: type checking failed ; expecting `VLam`, `HLam` or `VCon` ; got `" <> show v <> "`.\nPlease report the issue."
evaluate (EMatch expr pats :- _) = evaluate expr >>= \e -> foldr ((<|>) . uncurry (evalBranch e)) makeMatchError pats
evaluate (EHole :- _) = throwError . text $ "Developer error: type checking failed ; unexpected type hole while executing.\nPlease report the issue."
evaluate (EAnn e _ :- _) = evaluate e

-- | Evaluates a pattern matching branch (@pattern -> expression@).
evalBranch :: Value -> Annotated Pattern -> Annotated Expr -> EvalEnv Value
evalBranch e pat branch = do
    s <- unpackPattern e pat
    local (vals %~ (s <>)) (evaluate branch)

-- | Unpacks a pattern from a value.
--
-- Returns a new scope with the new bindings for the variables in the pattern.
unpackPattern :: Value -> Annotated Pattern -> EvalEnv (Scope Value)
unpackPattern = curry $ \case
    (_, Wildcard :- _)               -> pure mempty
    (VInt n, PInt n' :- _) | n == n' -> pure mempty
    (VDec d, PDec d' :- _) | d == d' -> pure mempty
    (v, PId id' :- _)                -> pure $ Map.singleton id' v
    (VCon id' v, PCtor id'' v' :- _)
        | id' == id''                -> mconcat <$> zipWithM unpackPattern v v'
    (val, PAnn p _ :- _)             -> unpackPattern val p
    (val, PLinear p :- _)            -> unpackPattern val p
    (VTuple vs, PTuple ps :- _)
        | length vs == length ps     -> mconcat <$> zipWithM unpackPattern vs ps
    _                                -> empty

makeMatchError :: EvalEnv a
makeMatchError = throwError $ text "Non-exhaustive patterns in pattern matching" <> dot <> linebreak