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

{-# LANGUAGE TypeApplications, TypeFamilies #-}

module Language.Nihil.TypeChecking where

import Language.Nihil.TypeChecking.KindChecker (KI, KIError)
import Language.Nihil.TypeChecking.Internal.Defaults.KindState (initKindState)
import Language.Nihil.TypeChecking.Solver.KindSolver (runKindSolver)
import Language.Nihil.TypeChecking.Internal.Substitution (apply, Substitutable, Subst)
import Language.Nihil.TypeChecking.Internal.Substitution.Kinds (KindSubst)
import Language.Nihil.TypeChecking.Internal.Environment (KindEnv, GlobalEnv)
import Language.Nihil.TypeChecking.TypeChecker (TIError, Check)
import Control.Monad.Except (runExcept)
import Control.Monad.RWS (evalRWST)
import Control.Monad.State (runStateT)

runKI :: (Substitutable a, Subst a ~ KindSubst) => KindEnv -> KI a -> Either KIError a
runKI env k = do
    (kind, c) <- runExcept (evalRWST k env initKindState)
    sub       <- runKindSolver env c
    pure (apply sub kind)

runCheck :: GlobalEnv -> Check a -> Either TIError (a, GlobalEnv)
runCheck e c = runExcept (runStateT c e)