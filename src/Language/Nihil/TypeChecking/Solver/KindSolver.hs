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

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts
           , TypeApplications, TypeFamilies #-}

module Language.Nihil.TypeChecking.Solver.KindSolver where

import Language.Nihil.TypeChecking.KindChecker (KIError, Solve)
import Language.Nihil.TypeChecking.Internal.Substitution.Kinds (KindSubst)
import Language.Nihil.TypeChecking.Internal.Constraint (KindConstraint(..))
import Language.Nihil.TypeChecking.Internal.Unification (unify)
import Language.Nihil.TypeChecking.Internal.Substitution
import Language.Nihil.TypeChecking.Rules.Kinds.Unify ()
import Language.Nihil.TypeChecking.Internal.Environment (KindEnv)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReader)
import qualified Data.Set as Set

solve :: (KindSubst, [KindConstraint]) -> Solve KindSubst
solve (su, cs) = case cs of
    []                -> pure su
    ((t1 :*~: t2):cs) -> do
        su' <- unify t1 t2
        solve (su' <> su, apply su' cs)

runKindSolver :: KindEnv -> [KindConstraint] -> Either KIError KindSubst
runKindSolver ke cs = runReader (runExceptT (solve st)) ke
  where st = (mempty, cs)

instance Substitutable KindConstraint where
    type Subst KindConstraint = KindSubst

    fv (a :*~: b)      = fv a `Set.union` fv b
    apply s (a :*~: b) = apply s a :*~: apply s b