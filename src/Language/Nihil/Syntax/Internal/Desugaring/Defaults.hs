-- The Great Nihil Compiler
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

module Language.Nihil.Syntax.Internal.Desugaring.Defaults where

import Language.Nihil.Syntax.Desugarer (SugarState(..))
import qualified Data.Map as Map
import qualified Language.Nihil.Syntax.Internal.Parsing.AST as P (Fixity(..), Associativity(..))

initSugarState :: SugarState
initSugarState = SugarState $ Map.fromList
    [ ("*", P.Infix P.L 7 "*")
    , ("/", P.Infix P.L 7 "/")
    , ("+", P.Infix P.L 6 "+")
    , ("-", P.Infix P.L 6 "-")
    , (":", P.Infix P.L 5 ":") ]