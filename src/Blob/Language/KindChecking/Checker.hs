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

{-# LANGUAGE TupleSections, LambdaCase #-}

-- | This module holds the kind checker.
module Blob.Language.KindChecking.Checker where

import Blob.Language.TypeChecking.Types
import Blob.Language.KindChecking.Types
import Blob.Language.Pretty.Inference (pKind)

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust, maybe)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Lens

import Text.PrettyPrint.Leijen (text, linebreak, dot)

import Debug.Trace

-- | The empty substitution
nullKindSubst :: KindSubst
nullKindSubst = mempty

-- | Composes two substitutions into one.
composeKindSubst :: KindSubst -> KindSubst -> KindSubst
composeKindSubst s1 s2 = Map.map (applyKind s1) s2 `Map.union` s1

-- | Composes @n@ substitutions into one.
concatKindSubsts :: [KindSubst] -> KindSubst
concatKindSubsts = foldr composeKindSubst nullKindSubst

-- | Runs the kind checking algorithm.
runKI :: KindEnv -> KI a -> (Either KIError a, KIState)
runKI env ki = runState (runReaderT (runExceptT ki) env) initKIState
  where initKIState = KIState { _kiSupply = 0 }

-- | Generates a new kind variable with the prefix given as argument.
newKindVar :: String -> KI Kind
newKindVar prefix = do
    s <- use kiSupply
    kiSupply += 1
    pure $ KVar (prefix <> show s)

-- | Binds a kind variable to a kind
kindVarBind :: String -> Kind -> KI KindSubst
kindVarBind u k | k == KVar u          = pure nullKindSubst
                | u `Set.member` fkv k = throwError (makeKindOccurError u k)
                | otherwise            = pure $ Map.singleton u k

-- | The core of the unification algorithm.
--
-- Basically:
--
-- > unify (KVar n) k  = n `bind` k
-- > unify k (KVar n)  = n `bind` k
-- > unify KType KType = no substitution
-- > unify (KArr l r) (KArr l' r') = do
-- >     s1 <- unify l l'
-- >     s2 <- unify (apply s1 r) (apply s1 r')
-- >     s1 `compose` s2
mguKind :: Kind -> Kind -> KI KindSubst
mguKind (KVar n) k = kindVarBind n k
mguKind k (KVar n)= kindVarBind n k
mguKind KType KType = pure nullKindSubst
mguKind (KArr l r) (KArr l' r') = do
    s1 <- mguKind l l'                               -- unify the left hand side of the arrow
    s2 <- mguKind (applyKind s1 r) (applyKind s1 r') -- unify the right hand side of the arrow
    pure $ s1 `composeKindSubst` s2                  -- and compose the substitutions
mguKind k1 k2 = throwError (makeKindUnifyError k1 k2)

makeKindUnifyError :: Kind -> Kind -> KIError
makeKindUnifyError k1 k2 = text "Could not match kind \"" <> pKind k1 <> text "\" with \"" <> pKind k2 <> text "\"" <> dot <> linebreak
makeKindOccurError :: String -> Kind -> KIError
makeKindOccurError s k1 = text "Occur check fails: kind " <> text s <> text " vs " <> pKind k1 <> dot <> linebreak
makeUndefinedTypeError :: String -> KIError
makeUndefinedTypeError s = text "Undefined kind of type \"" <> text s <> text "\"" <> dot <> linebreak
makeRedeclaredTypeError :: String -> KIError
makeRedeclaredTypeError id' = text "Type \"" <> text id' <> text "\" has already been declared" <> dot <> linebreak

-- | Kind checking for custom types.
kiCustomScheme :: CustomScheme -> KI (KindSubst, Kind)
kiCustomScheme (CustomScheme tvs t) = do
    typeArgs <- Map.fromList <$> mapM (\ v -> (v,) <$> newKindVar "k") tvs
    (s,k') <- local (Map.union typeArgs) $ case t of
        -- If it is a sum type, kind checking all of its constructors
        TSum constrs -> (,KType) <$> kiConstrs (Map.toList constrs)
        -- If it is a type alias, kind check the type
        TAlias t -> kiType t
        _ -> undefined

    let k = foldr KArr k' (fromJust . flip Map.lookup typeArgs <$> tvs)
    pure (s, applyKind s k)
  where foldConstr (TFun (t1, _) t2) = t1 : foldConstr t2
        foldConstr t = []

        kiConstrs [] = pure nullKindSubst
        kiConstrs ((n, Scheme _ c):cs) = do
            s1 <- kiConstr n (foldConstr c)
            s2 <- kiConstrs cs
            pure (s2 `composeKindSubst` s1)

        kiConstr _ [] = pure nullKindSubst
        kiConstr n (t:ts) = do
            (s1, k) <- kiType t
            s2 <- mguKind k KType
            s3 <- kiConstr n ts
            pure (concatKindSubsts [s3,s2,s1])

-- | Transforms a 'Type' into a pair composed of a 'KindSubst' and a 'Kind'.
kiType :: Type -> KI (KindSubst, Kind)
kiType (TId n)       = asks (Map.lookup n) >>= maybe err (pure . (mempty,))
    where err = throwError (makeUndefinedTypeError n)
kiType (TVar (TV n)) = asks (Map.lookup n) >>= maybe err (pure . (mempty,))
    where err = throwError (makeUndefinedTypeError n)
kiType (TRigid (TV n)) = asks (Map.lookup n) >>= maybe err (pure . (mempty,))
    where err = throwError (makeUndefinedTypeError n)
kiType (TTuple []) = pure (mempty, KType)
kiType (TTuple (t:ts)) = do
    (s1, k) <- kiType t
    s2 <- mguKind k KType
    (s3, _) <- kiType (TTuple ts)
    pure (concatKindSubsts [s3,s2,s1], KType)
kiType (TFun (t1, _) t2) = do
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

-- | Transforms a 'Scheme' into a pair composed of a 'KindSubst' and a 'Kind'.
kiScheme :: Scheme -> KI (KindSubst, Kind)
kiScheme (Scheme vars t) = do
    nvars <- mapM (const $ newKindVar "k") vars
    let s = Map.fromList (zipWith (\(TV v) n -> (v, n)) vars nvars)
    local (Map.union s) (kiType t)

-- | Run a 'KI' monad inside the 'Check' monad.
checkKI :: KI a -> Check a
checkKI ki = do
    (GlobalEnv env _ _ _) <- get
    let res = runKI env ki
    case fst res of
        Left err     -> throwError err
        Right result -> pure result

-- | Infer the 'Kind' of a 'Type'.
kindInference :: KindEnv -> Type -> KI Kind
kindInference _ e = do
    (s, k) <- kiType e
    pure $ applyKind s k
