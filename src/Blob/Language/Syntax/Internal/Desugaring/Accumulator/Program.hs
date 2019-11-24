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

module Blob.Language.Syntax.Internal.Desugaring.Accumulator.Program where

import Blob.Language.Syntax.Desugarer (Desugarer)
import Blob.Language.Syntax.Internal.Parsing.Located
import qualified Blob.Language.Syntax.Internal.Parsing.AST as P
import Blob.Language.Syntax.Internal.Desugaring.Accumulator.Statement

-- | Accumulates operator fixities in a 'P.Program', beginning with 'P.OpFixity' statements.
accumulateOnProgram :: Located P.Program -> Desugarer ()
accumulateOnProgram (p :@ _) = do
    let customOps = filter check p
        other = filter (not . check) p

    mapM_ accumulateOnStatement customOps
    mapM_ accumulateOnStatement other
  where check (P.OpFixity{} :@ _) = True
        check _ = False