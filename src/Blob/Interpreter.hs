-- Blobc, a compiler for compiling Blob source code
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

module Blob.Interpreter where

import Blob.Interpreter.Evaluator
import Blob.Interpreter.Value
import Blob.Interpreter.Errors.IncompleteMatch
import Blob.Interpreter.Scope
import Blob.Language (Expr(..), Pattern(..), Located(..), Literal(..), Statement(..))
import Control.Monad.Reader (runReaderT, local)
import Control.Monad.Except (throwError, runExceptT)
import Control.Monad (zipWithM, forM)
import Text.PrettyPrint.ANSI.Leijen (text, pretty)
import Control.Applicative ((<|>), empty)
import Control.Lens (views, (%~), view, (^.))
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Composition ((.:))

-- | Evaluates an expression.
evaluate :: Located Expr -> Eval Value
evaluate (ELit (LInt v) :@ _)    = pure $ VInt v
evaluate (ELit (LDec v) :@ _)    = pure $ VDec v
evaluate (ELit (LChr v) :@ _)    = pure $ VChr v
evaluate (EId id' :@ _)          =
    vals `views` (Map.lookup id' . (^. _Scope)) >>= \case
        Nothing -> -- no function in this scope, checking constructors
            ctors `views` (id' `elem`) >>= \case
                False -> throwError . text $ "Developer error: type checking failed; expecting name \"" <> id' <> "\" to be bound.\nPlease report the issue."
                True  -> pure (VCon id' [])
        Just x  -> pure x
evaluate (ETuple es :@ _)        = VTuple <$> mapM evaluate es
evaluate (ELam x e :@ _)         = VLam x e <$> view vals
    -- Do not unwrap the lambda, as an argument has not been given yet
evaluate (ELet ss e :@ _)    = do
    env' <- (catMaybes <$>) . forM ss $ \case
        Definition name expr :@ _ -> Just . (name,) <$> evaluate expr
        _ -> pure Nothing
    local (vals %~ (Scope (Map.fromList env') <>)) $
        evaluate e
evaluate (EApp f x :@ _)         = do
    x' <- evaluate x
    f' <- evaluate f
    case f' of
        VLam x e c -> do
            vals' <- unpackPattern x' x <|> throwError makeMatchError
            local (vals %~ ((vals' <> c) <>)) (evaluate e)
        HLam f''   -> f'' x'
        VCon id' e -> pure $ VCon id' (e <> [x'])
        v          -> throwError . text $ "Developer error: type checking failed; expecting `VLam`, `HLam` or `VCon` ; got `" <> show (pretty v) <> "`.\nPlease report the issue."
evaluate (EMatch expr pats :@ _) = do
    e <- evaluate expr
    foldr ((<|>) . uncurry (evalBranch e)) (throwError makeMatchError) pats
evaluate (EHole :@ _) = throwError . text $ "Developer error: type checking failed; unexpected type hole while executing.\nPlease report the issue."
evaluate (EAnn e _ :@ _) = evaluate e

-- | Evaluates a pattern matching branch (@pattern -> expression@).
evalBranch :: Value -> Located Pattern -> Located Expr -> Eval Value
evalBranch e pat branch = do
    s <- unpackPattern e pat
    local (vals %~ (s <>)) (evaluate branch)

-- | Unpacks a pattern from a value.
--
-- Returns a new scope with the new bindings for the variables in the pattern.
unpackPattern :: Value -> Located Pattern -> Eval (Scope Value)
unpackPattern = curry $ \case
    (_, Wildcard :@ _)               -> pure mempty
    (VInt n, PInt n' :@ _) | n == n' -> pure mempty
    (VDec d, PDec d' :@ _) | d == d' -> pure mempty
    (v, PId id' :@ _)                -> pure (Scope $ Map.singleton id' v)
    (VCon id' v, PCtor id'' v' :@ _)
        | id' == id''                -> mconcat <$> zipWithM unpackPattern v v'
    (val, PAnn p _ :@ _)             -> unpackPattern val p
    (VTuple vs, PTuple ps :@ _)
        | length vs == length ps     -> mconcat <$> zipWithM unpackPattern vs ps
    _                                -> empty

runEval' :: Located Expr -> EvalState -> IO (Either EvalError Value)
runEval' = runEval . evaluate

runEval :: Eval a -> EvalState -> IO (Either EvalError a)
runEval = runExceptT .: runReaderT