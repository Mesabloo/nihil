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

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts
           , TypeApplications #-}

module Blob.Language.TypeChecking.Solver.TypeSolver where

import Blob.Language.TypeChecking.TypeChecker (TIError, Solve)
import Blob.Language.TypeChecking.Internal.Substitution.Types (TypeSubst)
import Blob.Language.TypeChecking.Internal.Constraint (TypeConstraint(..))
import Blob.Language.TypeChecking.Internal.Unification (unify)
import Blob.Language.TypeChecking.Internal.Substitution
import Blob.Language.TypeChecking.Internal.Type (TVar)
import Blob.Language.TypeChecking.Rules.Types.Unify ()
import Blob.Language.TypeChecking.Internal.Environment (GlobalEnv)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReader)
import qualified Data.Set as Set

solve :: (TypeSubst, [TypeConstraint]) -> Solve TypeSubst
solve (su, cs) = case cs of
    []                -> pure su
    ((t1 :^~: t2):cs) -> do
        su' <- unify t1 t2
        solve (su' <> su, apply @_ @_ @TVar su' cs)

runTypeSolver :: GlobalEnv -> [TypeConstraint] -> Either TIError TypeSubst
runTypeSolver ge cs = runReader (runExceptT (solve st)) ge
  where st = (mempty, cs)

instance Substitutable TypeSubst TypeConstraint TVar where
    fv (a :^~: b)      = fv @TypeSubst a `Set.union` fv @TypeSubst b
    apply s (a :^~: b) = apply @_ @_ @TVar s a :^~: apply @_ @_ @TVar s b