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

module Blob.Language.Syntax.Internal.Desugaring.Accumulator.Expression where

import Blob.Language.Syntax.Desugarer (Desugarer, fixities)
import Blob.Language.Syntax.Internal.Parsing.Located
import qualified Blob.Language.Syntax.Internal.Parsing.AST as P
import {-# SOURCE #-} Blob.Language.Syntax.Internal.Desugaring.Accumulator.Statement
import Blob.Language.Syntax.Internal.Desugaring.Accumulator.Pattern
import Control.Lens ((%=), (^.))
import qualified Data.Map as Map

-- | Accumulates operator fixities in a 'P.Expr'.
accumulateOnExpression :: Located P.Expr -> Desugarer ()
accumulateOnExpression = mapM_ accumulateOnAtom . (^. located)

-- | Accumulates operator fixities in a 'P.Atom'.
--
-- In case of an unregistered operator being encountered, a default fixity of @infixl 9@ is registered.
accumulateOnAtom :: Located P.Atom -> Desugarer ()
accumulateOnAtom (P.AOperator name :@ _) = fixities %= Map.insertWith (flip const) name (P.Infix P.L 9 name)
accumulateOnAtom (P.AList e :@ _) = mapM_ accumulateOnExpression e
accumulateOnAtom (P.ATuple e :@ _) = mapM_ accumulateOnExpression e
accumulateOnAtom (P.ALambda _ e :@ _) = accumulateOnExpression e
accumulateOnAtom (P.AMatch e pats :@ _) = do
    accumulateOnExpression e
    mapM_ (accumulateOnPatterns . fst) pats
    mapM_ (accumulateOnExpression . snd) pats
accumulateOnAtom (P.AParens e :@ _) = accumulateOnExpression e
accumulateOnAtom (P.AApp a1 a2 :@ _) = do
    accumulateOnAtom a1
    accumulateOnAtom a2
accumulateOnAtom (P.AAnn e t :@ _) = accumulateOnExpression e
accumulateOnAtom (P.ALet stts e :@ _) =
    mapM_ accumulateOnStatement stts *> accumulateOnExpression e
accumulateOnAtom _ = pure ()