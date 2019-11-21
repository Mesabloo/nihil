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

{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes, FlexibleInstances #-}

module Blob.Language.TypeChecking.Internal.Substitution where

import qualified Data.Set as Set

-- | A type class for the substitutable types.
class Substitutable subst a var where
    -- | Applies a substitution to the given type.
    apply :: subst -> a -> a
    -- | Gets the free type variables (those which can be substituted) from the given type.
    fv    :: a -> Set.Set var