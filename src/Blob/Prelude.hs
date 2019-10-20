-- The Blob programming language's standard library
-- Copyright (c) 2019 Mesabloo

-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--   * Redistributions of source code must retain the above copyright
--     notice, this list of conditions and the following disclaimer.
--   * Redistributions in binary form must reproduce the above copyright
--     notice, this list of conditions and the following disclaimer in
--     the documentation and/or other materials provided with the
--     distribution.
--   * Neither the name of Mesabloo nor the names of its
--     contributors may be used to endorse or promote products derived
--     from this software without specific prior written permission.

{-# LANGUAGE LambdaCase #-}

-- | This module holds all the built-in functions/types from the language.
module Blob.Prelude where

import Blob.Interpreter.Types (Value(..), Scope, EvalState(..))
import Blob.Language.TypeChecking.Types
import qualified Data.Map as Map
import Text.PrettyPrint.Leijen (text)
import Control.Monad.Except (throwError)

-- | The default environment used when evaluatingan expression.
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

-- | The default types of the built-in functions.
defaultDeclContext :: Map.Map String Scheme
defaultDeclContext = Map.fromList [ ("+", Scheme [TV "a"] $ TFun (TVar $ TV "a", 1) (TFun (TVar $ TV "a", 1) (TVar $ TV "a")))
                                    -- (+) :: a|1| -> a|1| -> a
                                  , ("-", Scheme [TV "a"] $ TFun (TVar $ TV "a", 1) (TFun (TVar $ TV "a", 1) (TVar $ TV "a")))
                                    -- (-) :: a|1| -> a|1| -> a
                                  , ("*", Scheme [TV "a"] $ TFun (TVar $ TV "a", 1) (TFun (TVar $ TV "a", 1) (TVar $ TV "a")))
                                    -- (*) :: a|1| -> a|1| -> a
                                  , ("/", Scheme [TV "a"] $ TFun (TVar $ TV "a", 1) (TFun (TVar $ TV "a", 1) (TVar $ TV "a")))
                                    -- (/) :: a|1| -> a|1| -> a
                                  ]

-- | A duplicate of the above default environment.
defaultDefContext :: Map.Map String Scheme
defaultDefContext = defaultDeclContext

-- | The default kinds of the built-in types.
defaultTypeDeclContext :: Map.Map String Kind
defaultTypeDeclContext = Map.fromList [ ("Integer", KType)
                                            -- Integer :: *
                                      , ("Float",   KType)
                                            -- Float :: *
                                      , ("Char",    KType)
                                            -- Char :: *
                                      , ("[]",      KType `KArr` KType)
                                            -- ([]) :: * -> *
                                      , ("()",      KType)
                                            -- (()) :: *
                                      ]

-- | The default types of the built-in data type constructors.
defaultCtorsContext :: Map.Map String Scheme
defaultCtorsContext = Map.fromList [ (":", Scheme [TV "a"] $ TFun (TVar $ TV "a", 1) (TFun (TApp (TId "[]") $ TVar $ TV "a", 1) (TApp (TId "[]") . TVar $ TV "a")))
                                        -- (:) :: a|1| -> [a]|1| -> [a]
                                   , ("[]", Scheme [TV "a"] $ TApp (TId "[]") (TVar $ TV "a"))
                                        -- ([]) :: [a]
                                   , ("()", Scheme [] $ TId "()")
                                        -- (()) :: ()
                                   ]

-- | All the built-in Types with their constructors.
defaultTypeDefContext :: Map.Map String CustomScheme
defaultTypeDefContext = Map.fromList [ ("[]", CustomScheme ["a"] . TSum $
                                                    Map.fromList [ ("[]", defaultCtorsContext Map.! "[]")
                                                                 , ( ":", defaultCtorsContext Map.! ":") ])
                                     , ("()", CustomScheme [] . TSum $
                                                    Map.fromList [ ("()", defaultCtorsContext Map.! "()") ])
                                     , ("Integer", CustomScheme [] (TAlias TInt))
                                     , ("Double", CustomScheme [] (TAlias TFloat))
                                     , ("Char", CustomScheme [] (TAlias TChar)) ]

-- | The default 'GlobalEnv' for type checking.
initGlobalEnv :: GlobalEnv
initGlobalEnv =
    GlobalEnv { _typeDeclCtx = defaultTypeDeclContext
              , _typeDefCtx  = defaultTypeDefContext
              , _defCtx      = TypeEnv defaultDefContext
              , _ctorCtx     = TypeEnv defaultCtorsContext }

-- | The default 'EvalState' for the evaluation process.
initEvalState :: EvalState
initEvalState =
    EvalState { _vals  = defaultEnv
              , _ctors = Map.keys defaultCtorsContext }
