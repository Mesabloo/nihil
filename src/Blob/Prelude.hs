{-# LANGUAGE LambdaCase #-}

module Blob.Prelude where

import Blob.Interpreter.Types (Value(..), Scope, EvalState(..))
import Blob.Language.TypeChecking.Types
import qualified Data.Map as Map
import Text.PrettyPrint.Leijen (text)
import Control.Monad.Except (throwError)

defaultEnv :: Scope Value
defaultEnv = Map.fromList [ ("+", addF)
                          , ("-", subF)
                          , ("*", mulF)
                          , ("/", divF)
                          , ("kill", killF)
                          , ("dupl", duplF)
                          , ("read", readF)
                          , ("make", makeF) ]
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

    killF = HLam $ \_ -> pure $ VTuple []
    duplF = HLam $ \x -> pure $ VTuple [x, x]
    readF = HLam $ \x -> pure x
    makeF = HLam $ \x -> pure x


defaultDeclContext :: Map.Map String Scheme
defaultDeclContext = Map.fromList [ ("+", Scheme [TV "a"] $ TFun (TVar $ TV "a") (TFun (TVar $ TV "a") (TVar $ TV "a")))
                                  , ("-", Scheme [TV "a"] $ TFun (TVar $ TV "a") (TFun (TVar $ TV "a") (TVar $ TV "a")))
                                  , ("*", Scheme [TV "a"] $ TFun (TVar $ TV "a") (TFun (TVar $ TV "a") (TVar $ TV "a")))
                                  , ("/", Scheme [TV "a"] $ TFun (TVar $ TV "a") (TFun (TVar $ TV "a") (TVar $ TV "a")))
                                  , ("kill", Scheme [TV "a"] $ TBang (TVar $ TV "a") `TFun` TTuple [])
                                  , ("dupl", Scheme [TV "a"] $ TBang (TVar $ TV "a") `TFun` TTuple [TBang (TVar $ TV "a"), TBang (TVar $ TV "a")])
                                  , ("read", Scheme [TV "a"] $ TBang (TVar $ TV "a") `TFun` TVar (TV "a"))
                                  , ("make", Scheme [TV "a"] $ (TVar $ TV "a") `TFun` TBang (TVar $ TV "a")) ]

defaultDefContext :: Map.Map String Scheme
defaultDefContext = defaultDeclContext

defaultTypeDeclContext :: Map.Map String Kind
defaultTypeDeclContext = Map.fromList [ ("Integer", KType)
                                      , ("Float",   KType)
                                    --   , ("String",  KType)
                                      , ("Char",    KType)
                                      , ("[]",      KType `KArr` KType)
                                    --   , ("Either",  KType `KArr` KType `KArr` KType)
                                    --   , ("Maybe",   KType `KArr` KType)
                                      , ("()",      KType) ]

defaultCtorsContext :: Map.Map String Scheme
defaultCtorsContext = Map.fromList [ (":", Scheme [TV "a"] $ TFun (TVar $ TV "a") (TFun (TApp (TId "[]") $ TVar $ TV "a") (TApp (TId "[]") . TVar $ TV "a")))
                                   , ("[]", Scheme [TV "a"] $ TApp (TId "[]") (TVar $ TV "a"))
                                --    , ("Left", Scheme [TV "a", TV "b"] $ TFun (TVar $ TV "a") (TApp (TApp (TId "Either") (TVar $ TV "a")) (TVar $ TV "b")))
                                --    , ("Right", Scheme [TV "a", TV "b"] $ TFun (TVar $ TV "b") (TApp (TApp (TId "Either") (TVar $ TV "a")) (TVar $ TV "b")))
                                --    , ("Just", Scheme [TV "a"] $ TFun (TVar $ TV "a") (TApp (TId "Maybe") (TVar $ TV "a")))
                                --    , ("Nothing", Scheme [TV "a"] $ TApp (TId "Maybe") (TVar $ TV "a"))
                                   , ("()", Scheme [] $ TId "()") ]

defaultTypeDefContext :: Map.Map String CustomScheme
defaultTypeDefContext = Map.fromList [ ("[]", CustomScheme ["a"] . TSum $
                                                    Map.fromList [ ("[]", defaultCtorsContext Map.! "[]")
                                                                 , ( ":", defaultCtorsContext Map.! ":") ])
                                    --  , ("Either", CustomScheme ["a", "b"] . TSum $
                                    --                 Map.fromList [ ("Left", defaultCtorsContext Map.! "Left")
                                    --                              , ("Right", defaultCtorsContext Map.! "Right") ])
                                    --  , ("Maybe", CustomScheme ["a"] . TSum $
                                    --                 Map.fromList [ ("Just", defaultCtorsContext Map.! "Just")
                                    --                              , ("Nothing", defaultCtorsContext Map.! "Nothing") ])
                                    --  , ("String", CustomScheme [] . TAlias $ TApp (TId "[]") TChar)
                                     , ("()", CustomScheme [] . TSum $
                                                    Map.fromList [ ("()", defaultCtorsContext Map.! "()") ])
                                     , ("Integer", CustomScheme [] (TAlias TInt))
                                     , ("Double", CustomScheme [] (TAlias TFloat))
                                     , ("Char", CustomScheme [] (TAlias TChar)) ]

initGlobalEnv :: GlobalEnv
initGlobalEnv =
    GlobalEnv { typeDeclCtx = defaultTypeDeclContext
              , typeDefCtx  = defaultTypeDefContext
              , defCtx      = TypeEnv defaultDefContext
              , ctorCtx     = TypeEnv defaultCtorsContext }

initEvalState :: EvalState
initEvalState =
    EvalState { vals  = defaultEnv
              , ctors = Map.keys defaultCtorsContext }
