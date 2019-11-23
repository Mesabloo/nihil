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

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeApplications #-}

module Blob.Language.TypeChecking.Rules.Kinds.Unify where

import Blob.Language.TypeChecking.Internal.Unification
import Blob.Language.TypeChecking.Internal.Kind
import Blob.Language.TypeChecking.Internal.Substitution.Kinds
import Blob.Language.TypeChecking.KindChecker (Solve)
import Blob.Language.PrettyPrinting.Kinds ()
import Blob.Language.TypeChecking.Internal.Errors.InfiniteKind
import Blob.Language.TypeChecking.Internal.Substitution (fv)
import Blob.Language.TypeChecking.Internal.Errors.Unification (makeUnifyError)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Except (throwError)

instance Unifiable Kind Solve where
    unify (KVar (KV n)) k           = bind n k
    unify k (KVar (KV n))           = bind n k
    unify KType KType               = pure mempty
    unify (KArr l1 r1) (KArr l2 r2) =
        unifyMany [l1, l2] [r1, r2]
    unify k1 k2                     =
        throwError (makeUnifyError k1 k2)

bind :: String -> Kind -> Solve KindSubst
bind u k | k == KVar (KV u)    = pure mempty
         | u `Set.member` fv k = throwError $ makeInfiniteKindError u k
         | otherwise           = pure (Subst $ Map.singleton u k)