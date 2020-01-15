{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Nihil.Runtime.Interpreter
( evaluate ) where

import Nihil.Syntax
import Nihil.Runtime.Core
import Nihil.Utils.Source (annotated)
import Nihil.Runtime.Errors.NonExhaustivePatternMatching
import Nihil.Runtime.Errors.Developer
import Control.Arrow ((>>>))
import Data.Bool (bool)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Lens (views, view, (%~))
import Control.Monad (forM, zipWithM)
import Control.Monad.Reader (local)
import Control.Monad.Except (throwError)
import Control.Applicative ((<|>), empty)
import Prelude hiding (lookup)

-- | Evaluates an expression and returns its value.
evaluate :: Expr -> Eval Value
evaluate = annotated >>> eval

-- | Evaluates a raw expression (with no location).
eval :: Expr' -> Eval Value
eval (ELiteral lit)         = evalLiteral lit
eval (EId name)             =
    vals `views` lookup name >>= \case
        Nothing                ->
            cons `views` Set.member name >>= bool err (pure (VConstructor name []))
          where err = developerError ("Name \"" <> name <> "\" unbound.")
        Just (VUnevaluated ex) -> evaluate ex
        Just x                 -> pure x
eval (ETuple es)            = VTuple <$> mapM evaluate es
eval (ELambda arg ex)       = VLambda arg ex <$> view vals
eval (ELet ss e)            = do
    stts <- catMaybes <$> forM (annotated <$> ss) \case
        FunctionDefinition name ex -> pure (Just (name, VUnevaluated ex))
        _                          -> pure Nothing
    inEnvMany stts (evaluate e)
eval (ETypeAnnotated ex _)  = evaluate ex
eval (EApplication fun arg) = do
    arg' <- evaluate arg
    evaluate fun >>= \case
        VLambda argument ex ctx -> local (vals %~ mappend ctx) (evalCase arg' argument ex <|> throwError noMorePatterns)
        VPrim f                 -> f arg'
        VConstructor name es    -> pure (VConstructor name (es <> [arg']))
        _                       -> developerError "First argument in application should be a function."
eval (EMatch ex cases)      = do
    expr <- evaluate ex
    foldr ((<|>) . uncurry (evalCase expr)) (throwError noMorePatterns) cases
eval ETypeHole              = developerError "Remaining type hole."

-- | Evaluates a literal
evalLiteral :: Literal -> Eval Value
evalLiteral (LInteger i)   = pure (VInteger i)
evalLiteral (LFloat d)     = pure (VDouble d)
evalLiteral (LCharacter c) = pure (VCharacter c)

-- | Evaluates a pattern matching case.
evalCase :: Value -> Pattern -> Expr -> Eval Value
evalCase val pat expr = do
    env <- unpackPattern val pat
    local (vals %~ mappend env) (evaluate expr)

unpackPattern :: Value -> Pattern -> Eval (Scope Value)
unpackPattern v p = case (v, annotated p) of
    (_, PWildcard)                       -> pure mempty
    (VInteger n, PLiteral (LInteger n'))
        | n == n'                        -> pure mempty
    (VDouble d, PLiteral (LFloat d'))
        | d == d'                        -> pure mempty
    (v, PId name)                        -> pure (Scope (Map.singleton name v))
    (VConstructor name vs, PConstructor name' ps)
        | name == name'                  -> mconcat <$> zipWithM unpackPattern vs ps
    (val, PTypeAnnotated p _)            -> unpackPattern val p
    (VTuple vs, PTuple ps)               -> mconcat <$> zipWithM unpackPattern vs ps
    (VUnevaluated ex, _)                 -> evaluate ex >>= flip unpackPattern p
    _                                    -> empty

-----------------------------------------------------------------------------------------------------------------------

-- | Executes an action with some values added to the environment.
inEnvMany :: [(String, Value)] -> Eval a -> Eval a
inEnvMany env = local (vals %~ (Scope (Map.fromList env) <>))