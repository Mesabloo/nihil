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

{-# LANGUAGE TemplateHaskell #-}

module Blob.Language.TypeChecking.Internal.Type where

import Control.Lens (makePrisms)
import qualified Data.Map as Map

-- | A simple wrapper for a type variable.
newtype TVar = TV String
    deriving (Eq, Ord, Show)

makePrisms ''TVar

-- | A data type for representing various types, such as:
data Type
    = TVar TVar       -- ^ A substitutable type variable
    | TRigid TVar     -- ^ A rigid (non-substitutable) type variable
    | TInt            -- ^ The integer type (which is an alias defined in 'Blob.Prelude.defaultTypeDefContext')
    | TFloat          -- ^ The float type (also an alias defined in 'Blob.Prelude.defaultTypeDefContext')
    | TChar           -- ^ The char type (also an alias defined in 'Blob.Prelude.defaultTypeDefContext')
    | TFun (Type, Integer) Type  -- ^ A linear function
    | TTuple [Type]   -- ^ A tuple
    | TApp Type Type  -- ^ A type application
    | TId String      -- ^ A type identifier
    deriving (Eq, Ord, Show)

-- | A simple data type for the schme of a 'Type'
--
-- A scheme is composed of a 'Type' and its associated 'TVar's. It can be compared with @forall@.
data Scheme = Scheme [TVar] Type
    deriving (Eq, Ord, Show)

makePrisms ''Scheme

-- | A holder for a custom type.
data CustomType
    = TSum (Map.Map String Scheme)  -- ^ For a sum type, holds the type of each constructor
    | TAlias Type                   -- ^ For a type alias, holds the type of the aliased type
    deriving (Eq, Ord, Show)

-- | A holder for the scheme of a 'CustomType'.
data CustomScheme
    = CustomScheme [String] CustomType
    deriving (Eq, Ord, Show)

-- | A type alias for a 'Customtype' environment. Same as the 'TypeEnv' but holds 'CustomScheme's instead.
type CustomTypeEnv = Map.Map String CustomScheme
