{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Nihil.Runtime
( evaluate
, defaultEvalEnv
  -- * Re-exports
, module Nihil.Runtime.Pretty
, module Nihil.Runtime.Core ) where

import Nihil.Runtime.Core
import Nihil.Runtime.Pretty
import qualified Nihil.Runtime.Interpreter as I
import Nihil.Runtime.Errors.Developer
import Nihil.Syntax (Expr)
import Text.PrettyPrint.ANSI.Leijen (Doc)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import qualified Data.Set as Set
import qualified Data.Map as Map

-- | Evaluates an 'Expr'ession given an initial environment.
evaluate :: Expr -> EvalState -> IO (Either Doc Value)
evaluate ex env = runExceptT (runReaderT (I.evaluate ex) env)

defaultEvalEnv :: EvalState
defaultEvalEnv = EState (Scope defaultFunctions) defaultConstructors
  where defaultConstructors = Set.fromList
            ["()", "Nil", "Cons"]

        defaultFunctions = Map.fromList
            [ ("+", addValues)
            , ("-", subValues)
            , ("/", divValues)
            , ("*", mulValues)
            ]

numberOperation :: (forall a. Num a => a -> a -> a) -> Value
numberOperation op = VPrim \case
    VInteger x ->
        let f = \case
                VInteger y -> pure (VInteger (op x y))
                VDouble y  -> pure (VDouble (op (fromInteger x) y))
                _          -> developerError "Expected integer or float"
        in pure (VPrim f)
    VDouble x  ->
        let f = \case
                VInteger y -> pure (VDouble (op x (fromInteger y)))
                VDouble y  -> pure (VDouble (op x y))
                _          -> developerError "Expected integer or float"
        in pure (VPrim f)
    _          -> developerError "Expected integer or float"

fracOperation :: (forall a. Fractional a => a -> a -> a) -> Value
fracOperation op = VPrim \case
    VInteger x ->
        let f = \case
                VInteger y -> pure (VDouble (op (realToFrac x) (realToFrac y)))
                VDouble y  -> pure (VDouble (op (fromInteger x) y))
                _          -> developerError "Expected integer or float"
        in pure (VPrim f)
    VDouble x  ->
        let f = \case
                VInteger y -> pure (VDouble (op x (fromInteger y)))
                VDouble y  -> pure (VDouble (op x y))
                _          -> developerError "Expected integer or float"
        in pure (VPrim f)
    _          -> developerError "Expected integer or float"

addValues, subValues, divValues, mulValues :: Value
addValues = numberOperation (+)
subValues = numberOperation (-)
divValues = fracOperation (/)
mulValues = numberOperation (*)