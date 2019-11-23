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

module Blob.Language.TypeChecking.Solver.TypeHoleSolver where

import Blob.Language.TypeChecking.Internal.Substitution.Types
import Blob.Language.TypeChecking.Internal.Type
import Blob.Language.TypeChecking.TypeChecker (TIError)
import Blob.Language.TypeChecking.Internal.Errors.TypeHole
import qualified Data.Map as Map
import Control.Monad.Except (throwError)
import Text.PrettyPrint.ANSI.Leijen (empty)

-- | Runs the type hole solver.
runHoleInspect :: TypeSubst -> Either TIError ()
runHoleInspect (Subst subst) =
    let map' = Map.filterWithKey (\(TV k) _ -> head k == '_') subst
    in if null map'
        then pure ()
        else throwError $ Map.foldl (\acc t -> acc <> let (Scheme _ st) = closeOver t in makeHoleError st) empty map'
