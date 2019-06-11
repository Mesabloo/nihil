{-# LANGUAGE TupleSections, LambdaCase #-}

module Blob.KindChecking.Checker where

import Blob.Inference.Types
import qualified Blob.Parsing.Types as PT
import Blob.KindChecking.Types
import Blob.PrettyPrinter.PrettyInference (pType, pKind)

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust, maybe)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Text.PrettyPrint.Leijen (text, (<+>), linebreak, dot)

import Debug.Trace

nullKindSubst :: KindSubst
nullKindSubst = mempty

composeKindSubst :: KindSubst -> KindSubst -> KindSubst
composeKindSubst s1 s2 = Map.map (applyKind s1) s2 `Map.union` s1

concatKindSubsts :: [KindSubst] -> KindSubst
concatKindSubsts = foldr composeKindSubst nullKindSubst

runKI :: KindEnv -> KI a -> (Either KIError a, KIState)
runKI env ki = runState (runReaderT (runExceptT ki) env) initKIState
  where initKIState = KIState { kiSupply = 0 }

newKindVar :: String -> KI Kind
newKindVar prefix = do
    s <- get
    put s { kiSupply = kiSupply s + 1 }
    pure $ KVar (prefix <> show (kiSupply s))

kindVarBind :: String -> Kind -> KI KindSubst
kindVarBind u k | k == KVar u          = pure nullKindSubst
                | u `Set.member` fkv k = throwError (makeKindOccurError u k)
                | otherwise            = pure $ Map.singleton u k

mguKind :: Kind -> Kind -> KI KindSubst
mguKind (KVar n) k = kindVarBind n k
mguKind k (KVar n)= kindVarBind n k
mguKind KType KType = pure nullKindSubst
mguKind (KArr l r) (KArr l' r') = do
    s1 <- mguKind l l'
    s2 <- mguKind (applyKind s1 r) (applyKind s1 r')
    pure $ s1 `composeKindSubst` s2
mguKind k1 k2 = throwError (makeKindUnifyError k1 k2)

makeKindUnifyError :: Kind -> Kind -> KIError
makeKindUnifyError k1 k2 = text "Could not match kind “" <> pKind k1 <> text "” with “" <> pKind k2 <> text "”" <> dot <> linebreak
makeKindOccurError :: String -> Kind -> KIError
makeKindOccurError s k1 = text "Occur check fails: kind " <> text s <> text " vs " <> pKind k1 <> dot <> linebreak
makeUndefinedTypeError :: String -> KIError
makeUndefinedTypeError s = text "Undefined kind of type “" <> text s <> text "”" <> dot <> linebreak
makeRedeclaredTypeError :: String -> KIError
makeRedeclaredTypeError id' = text "Type “" <> text id' <> text "” already declared" <> dot <> linebreak

kiCustomScheme :: CustomScheme -> KI (KindSubst, Kind)
kiCustomScheme (CustomScheme tvs t) = do
    typeArgs <- Map.fromList <$> mapM (\ v -> (v,) <$> newKindVar "k") tvs
    s <- local (Map.union typeArgs) $ case t of
        TSum constrs -> kiConstrs (Map.toList constrs)
        TProd c s -> kiConstrs [(c, s)]
        _ -> undefined
    let k = foldr KArr KType (fromJust . flip Map.lookup typeArgs <$> tvs)
    pure (s, applyKind s k)
  where foldConstr (TFun t1 t2) = t1 : foldConstr t2
        foldConstr t = []

        kiConstrs [] = pure nullKindSubst
        kiConstrs ((n, Scheme _ c):cs) = do
            s1 <- kiConstr n (foldConstr c)
            s2 <- kiConstrs cs
            pure (s2 `composeKindSubst` s1)

        kiConstr n [] = pure nullKindSubst
        kiConstr n (t:ts) = do
            (s1, k) <- kiType t
            s2 <- mguKind k KType
            s3 <- kiConstr n ts
            pure (concatKindSubsts [s3,s2,s1])

kiType :: Type -> KI (KindSubst, Kind)
kiType (TId n) = asks (Map.lookup n) >>= maybe err (pure . (mempty,))
    where err = throwError (makeUndefinedTypeError n)
kiType (TVar n) = asks (Map.lookup n) >>= maybe err (pure . (mempty,))
    where err = throwError (makeUndefinedTypeError n)
kiType (TRigidVar n) = asks (Map.lookup n) >>= maybe err (pure . (mempty,))
    where err = throwError (makeUndefinedTypeError n)
kiType (TTuple []) = pure (mempty, KType)
kiType (TTuple (t:ts)) = do
    (s1, k) <- kiType t
    s2 <- mguKind k KType
    (s3, _) <- kiType (TTuple ts) 
    pure (concatKindSubsts [s3,s2,s1], KType)
kiType (TFun t1 t2) = do
    (s1, k1) <- kiType t1
    (s2, k2) <- kiType t2
    s3 <- mguKind k1 KType
    s4 <- mguKind k2 KType
    pure (concatKindSubsts [s4,s3,s2,s1], KType)
kiType (TApp f t) = do
    kv       <- newKindVar "k"
    (s1, k1) <- kiType f
    (s2, k2) <- local (applyKind s1 <$>) (kiType t)
    s3       <- mguKind (applyKind s2 k1) (KArr k2 kv)
    pure (concatKindSubsts [s3,s2,s1], applyKind s3 kv)
kiType t = traceShow t undefined

kiScheme :: Scheme -> KI (KindSubst, Kind)
kiScheme (Scheme vars t) = do
    nvars <- mapM (\_ -> newKindVar "k") vars
    let s = Map.fromList (zip vars nvars)
    local (Map.union s) (kiType t)

checkKI :: KI a -> Check a
checkKI ki = do
    (GlobalEnv env _ _ _) <- get
    let res = runKI env ki
    case fst res of
        Left err     -> throwError err
        Right result -> pure result

kindInference :: KindEnv -> Type -> KI Kind
kindInference _ e = do
    (s, k) <- kiType e
    pure $ applyKind s k
