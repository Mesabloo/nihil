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

module Blob.Language.TypeChecking.Solver.KindSolver where

import Blob.Language.TypeChecking.KindChecker (KIError, Solve)
import Blob.Language.TypeChecking.Internal.Substitution.Kinds (KindSubst)
import Blob.Language.TypeChecking.Internal.Constraint (KindConstraint(..))
import Blob.Language.TypeChecking.Internal.Unification (unify)
import Blob.Language.TypeChecking.Internal.Substitution
import Blob.Language.TypeChecking.Rules.Kinds.Unify ()
import Blob.Language.TypeChecking.Internal.Environment (KindEnv)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReader)
import qualified Data.Set as Set

solve :: (KindSubst, [KindConstraint]) -> Solve KindSubst
solve (su, cs) = case cs of
    []                -> pure su
    ((t1 :*~: t2):cs) -> do
        su' <- unify t1 t2
        solve (su' <> su, apply @_ @_ @String su' cs)

runKindSolver :: KindEnv -> [KindConstraint] -> Either KIError KindSubst
runKindSolver ke cs = runReader (runExceptT (solve st)) ke
  where st = (mempty, cs)

instance Substitutable KindSubst a String => Substitutable KindSubst [a] String where
    fv    = foldr (Set.union . fv @KindSubst) mempty
    apply = fmap . apply @_ @_ @String

instance Substitutable KindSubst KindConstraint String where
    fv (a :*~: b)      = fv @KindSubst a `Set.union` fv @KindSubst b
    apply s (a :*~: b) = apply @_ @_ @String s a :*~: apply @_ @_ @String s b