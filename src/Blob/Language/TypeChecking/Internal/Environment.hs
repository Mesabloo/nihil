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

{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Blob.Language.TypeChecking.Internal.Environment where

import Blob.Language.TypeChecking.Internal.Kind
import Blob.Language.TypeChecking.Internal.Type
import qualified Data.Map as Map
import Control.Lens (makePrisms, makeLenses)

-- | A type alias for a 'Kind' environment. Same as 'TypeEnv' but holds 'Kind's instead.
newtype KindEnv = KindEnv (Map.Map String Kind)
  deriving (Show, Monoid, Semigroup)

makePrisms ''KindEnv

-- | The wrapper for a 'Type' environment.
--
-- It holds a mapping of variables with their 'Scheme's.
newtype TypeEnv = TypeEnv (Map.Map String Scheme)
    deriving (Show, Monoid, Semigroup)

makePrisms ''TypeEnv

-- | The typing environment used. It holds:
data GlobalEnv = GlobalEnv
    { _typeDeclCtx :: KindEnv        -- ^ A 'Kind' environment for type correctness checking
    , _typeDefCtx  :: CustomTypeEnv  -- ^ A 'CustomType' environment for data constructor existence checking
    , _defCtx      :: TypeEnv        -- ^ A 'Type' environment for type checking and function definition checking
    , _ctorCtx     :: TypeEnv        -- ^ A 'Type' environment for type checking and data constructor checking
    }
    deriving Show

makeLenses ''GlobalEnv


