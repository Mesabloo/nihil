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

{-# LANGUAGE LambdaCase #-}

-- | This module holds the default definition of constants used in the desugaring process.
module Blob.Language.Desugaring.Defaults where

import Blob.Language.Desugaring.Types
import qualified Data.Map as Map
import qualified Blob.Language.Parsing.Types as P (Fixity(..), Associativity(..))

-- | The default desugarer state
initSugarState :: SugarState
initSugarState =
    SugarState { _fixities = Map.fromList [ ("*", P.Infix P.L 7 "*") -- infixl 7 *
                                          , ("/", P.Infix P.L 7 "/") -- infixl 7 /
                                          , ("+", P.Infix P.L 6 "+") -- infixl 6 +
                                          , ("-", P.Infix P.L 6 "-") -- infixl 6 -
                                          , (":", P.Infix P.R 5 ":") -- infixr 5 :
                                          ]
               }