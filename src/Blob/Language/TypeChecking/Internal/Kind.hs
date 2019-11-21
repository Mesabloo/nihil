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

module Blob.Language.TypeChecking.Internal.Kind where

import Control.Lens (makePrisms)

newtype KVar = KV String
    deriving (Eq, Show, Ord)

makePrisms ''KVar

-- | A data type to represent the possible kinds in the language.
data Kind
    = KType           -- ^ A simple kind @*@
    | KArr Kind Kind  -- ^ A kind function @k1 -> k2@
    | KVar KVar       -- ^ A kind variable
    deriving (Eq, Show)
infixr 3 `KArr`
