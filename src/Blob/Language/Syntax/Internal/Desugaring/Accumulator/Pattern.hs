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

module Blob.Language.Syntax.Internal.Desugaring.Accumulator.Pattern where

import Blob.Language.Syntax.Desugarer (Desugarer, fixities)
import Blob.Language.Syntax.Internal.Parsing.Located
import qualified Blob.Language.Syntax.Internal.Parsing.AST as P
import Control.Lens ((%=))
import qualified Data.Map as Map

-- | Accumulates operator fixities in 'P.Pattern's.
accumulateOnPatterns :: [Located P.Pattern] -> Desugarer ()
accumulateOnPatterns = mapM_ accumulateOnPattern

-- | Accumulates operator fixities in a 'P.Pattern'.
accumulateOnPattern :: Located P.Pattern -> Desugarer ()
accumulateOnPattern (P.POperator name :@ _) =
    fixities %= Map.insertWith (flip const) name (P.Infix P.L 9 name)
accumulateOnPattern (P.PCtor _ ps :@ _) = mapM_ accumulateOnPatterns ps
accumulateOnPattern (P.PList ps :@ _) = mapM_ accumulateOnPatterns ps
accumulateOnPattern (P.PTuple ps :@ _) = mapM_ accumulateOnPatterns ps
accumulateOnPattern (P.PParens p :@ _) = accumulateOnPatterns p
accumulateOnPattern _ = pure ()