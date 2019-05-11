{-# LANGUAGE LambdaCase #-}

module Blob.REPL.DefaultEnv where

import Blob.REPL.Types (Value(..), Scope)
import Blob.Inference.Types (Scheme(..), Type(..), Kind(..))
import qualified Data.Map as Map
import Text.PrettyPrint.Leijen (text)
import Control.Monad.Except (throwError)

defaultEnv :: Scope Value
defaultEnv = Map.fromList [ ("+", addF)
                          , ("-", subF)
                          , ("*", mulF)
                          , ("/", divF) ]
  where
    addF = HLam $ \case
        VInt x -> pure . HLam $ \case
            VInt y -> pure $ VInt (x + y)
            VDec y -> pure $ VDec (fromInteger x + y)
            _      -> throwError (text "Expected integer or float")
        VDec x -> pure . HLam $ \case
            VInt y -> pure $ VDec (x + fromInteger y)
            VDec y -> pure $ VDec (x + y)
            _      -> throwError (text "Expected integer or float")
        _      -> throwError (text "Expected integer or float")

    subF = HLam $ \case
        VInt x -> pure . HLam $ \case
            VInt y -> pure $ VInt (x - y)
            VDec y -> pure $ VDec (fromInteger x - y)
            _      -> throwError (text "Expected integer or float")
        VDec x -> pure . HLam $ \case
            VInt y -> pure $ VDec (x - fromInteger y)
            VDec y -> pure $ VDec (x - y)
            _      -> throwError (text "Expected integer or float")
        _      -> throwError (text "Expected integer or float")

    mulF = HLam $ \case
        VInt x -> pure . HLam $ \case
            VInt y -> pure $ VInt (x * y)
            VDec y -> pure $ VDec (fromInteger x * y)
            _      -> throwError (text "Expected integer or float")
        VDec x -> pure . HLam $ \case
            VInt y -> pure $ VDec (x * fromInteger y)
            VDec y -> pure $ VDec (x * y)
            _      -> throwError (text "Expected integer or float")
        _      -> throwError (text "Expected integer or float")

    divF = HLam $ \case
        VInt x -> pure . HLam $ \case
            VInt y -> pure $ VDec (fromInteger x / fromInteger y)
            VDec y -> pure $ VDec (fromInteger x / y)
            _      -> throwError (text "Expected integer or float")
        VDec x -> pure . HLam $ \case
            VInt y -> pure $ VDec (x / fromInteger y)
            VDec y -> pure $ VDec (x / y)
            _      -> throwError (text "Expected integer or float")
        _      -> throwError (text "Expected integer or float")


defaultDeclContext :: Map.Map String Scheme
defaultDeclContext = Map.fromList [ ("+", Scheme ["a"] $ TFun (TRigidVar "a") (TFun (TRigidVar "a") (TRigidVar "a")))
                                  , ("-", Scheme ["a"] $ TFun (TRigidVar "a") (TFun (TRigidVar "a") (TRigidVar "a")))
                                  , ("*", Scheme ["a"] $ TFun (TRigidVar "a") (TFun (TRigidVar "a") (TRigidVar "a")))
                                  , ("/", Scheme ["a"] $ TFun (TRigidVar "a") (TFun (TRigidVar "a") (TRigidVar "a"))) ]

defaultDefContext :: Map.Map String Scheme
defaultDefContext = defaultDeclContext

defaultTypeDeclContext :: Map.Map String Kind
defaultTypeDeclContext = Map.fromList [ ("Integer", KType)
                                      , ("Float",   KType)
                                      , ("String",  KType) ]
