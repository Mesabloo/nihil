{-# LANGUAGE TupleSections #-}

module Blob.KindChecking.Checker where

import Blob.Inference.Types
import qualified Blob.Parsing.Types as PT
import Blob.KindChecking.Types

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Text.PrettyPrint.Leijen (text, (<+>))

nullKindSubst :: KindSubst
nullKindSubst = mempty

composeKindSubst :: KindSubst -> KindSubst -> KindSubst
composeKindSubst s1 s2 = Map.map (applyKind s1) s2 `Map.union` s1

concatKindSubsts :: [KindSubst] -> KindSubst
concatKindSubsts = foldr composeKindSubst nullKindSubst

runKI :: TypeEnv -> KI a -> (Either KIError a, KIState)
runKI env t = runState (runReaderT (runExceptT t) mempty) initKIState
  where initKIState = KIState { kiSupply  = 0
                              , kiTypeEnv = env }

newKindVar :: String -> KI Kind
newKindVar prefix = do
    s <- get
    put s { kiSupply = kiSupply s + 1 }
    pure $ KVar (prefix ++ show (kiSupply s))

mguKind :: Kind -> Kind -> KI KindSubst
mguKind KType KType = pure nullKindSubst
mguKind (KArr l r) (KArr l' r') = do
    s1 <- mguKind l l'
    s2 <- mguKind (applyKind s1 r) (applyKind s1 r')
    pure $ s1 `composeKindSubst` s2

kiType :: Type -> KI (KindSubst, Kind)
kiType (TId n) = gets (getScheme n . kiTypeEnv) >>= maybe err kiScheme
    where err = throwError (text "Undefined type" <+> text n)
kiType (TVar n) = asks (Map.lookup n) >>= maybe err (pure . (mempty,))
    where err = throwError (text "Unbound type variable" <+> text n)
kiType (TTuple []) = pure (mempty, KType)
kiType (TTuple (t:ts)) = do
    (s1, k) <- kiType t
    s2 <- mguKind k KType
    (s3, _) <- kiType (TTuple ts) 
    pure (concatKindSubsts [s3,s2,s1], KType)
kiType (TFun t1 t2) = do
    (s1, k1) <- kiType t1
    (s2, k2) <- kiType t1
    s3 <- mguKind k1 KType
    s4 <- mguKind k2 KType
    pure (concatKindSubsts [s4,s3,s2,s1], KType)
kiType (TList t) = do
    (s1, k) <- kiType t
    s2 <- mguKind k KType
    pure (s2 `composeKindSubst` s1, KType)
kiType (TApp f t) = do
    kv       <- newKindVar "k"
    (s1, k1) <- kiType f
    (s2, k2) <- local (applyKind s1 <$>) (kiType t)
    s3       <- mguKind (applyKind s2 k1) (KArr k2 kv)
    pure (s3 `composeKindSubst` s2 `composeKindSubst` s1, applyKind s3 kv)

kiScheme :: Scheme -> KI (KindSubst, Kind)
kiScheme (Scheme vars t) = do
    nvars <- mapM (\_ -> newKindVar "k") vars
    let s = Map.fromList (zip vars nvars)
    local (Map.union s) (kiType t)
