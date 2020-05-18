{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Nihil.TypeChecking.Rules.Unification.Kind
() where

import Nihil.TypeChecking.Unification
import Nihil.TypeChecking.Core
import Nihil.TypeChecking.Substitution
import Nihil.TypeChecking.Errors.Infinite (infiniteKind)
import Nihil.TypeChecking.Errors.Unification (unifyKind)
import Nihil.TypeChecking.Common
import Nihil.TypeChecking.Environment
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.Except (throwError)

instance Unifiable Kind KindEnv where
    unify k1 k2 | k1 == k2                          = pure mempty
    unify (KVar n)             k                    = bind n k
    unify k                    (KVar n)             = bind n k
    unify (KApplication k1 k2) (KApplication k3 k4) = unifyMany [k1, k2] [k3, k4]
    unify k1                   k2                   = throwError (unifyKind k1 k2)

-- | Tries to bind a kind variable to a kind (checking for infinite kinds).
bind :: String -> Kind -> SolveKind (Subst Kind)
bind u k | k == KVar u           = pure mempty
         | u `Set.member` free k = throwError (infiniteKind u k)
         | otherwise             = pure (Subst (Map.singleton u k))