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

{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances
           , TypeApplications, GeneralizedNewtypeDeriving, TypeFamilies #-}

module Blob.Language.TypeChecking.Internal.Substitution.Kinds where

import Blob.Language.TypeChecking.Internal.Kind
import Blob.Language.TypeChecking.Internal.Substitution
import qualified Data.Map as Map
import Control.Lens (makePrisms, (^.))
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

-- | The substitution type.
--
-- Its use is to store the kind of each type name.
newtype KindSubst = Subst (Map.Map String Kind)

makePrisms ''KindSubst

unpack :: KindSubst -> Map.Map String Kind
unpack = (^. _Subst)
{-# INLINE unpack #-}

instance Substitutable Kind where
    type Subst Kind = KindSubst

    fv KType          = Set.empty
    fv (k1 `KArr` k2) = fv k1 <> fv k2
    fv (KVar v)       = Set.singleton (v ^. _KV)

    apply _ KType          = KType
    apply s (KVar n)       = fromMaybe (KVar n) (Map.lookup (n ^. _KV) (unpack s))
    apply s (k1 `KArr` k2) = apply s k1 `KArr` apply s k2

instance Semigroup KindSubst where
    s1 <> s2 = Subst $ Map.map (apply s1) (unpack s2) `Map.union` unpack s1

instance Monoid KindSubst where
    mempty = Subst mempty