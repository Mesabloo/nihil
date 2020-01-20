module Nihil.TypeChecking.Rules.Solving.Kind where

import Nihil.TypeChecking.Unification
import Nihil.TypeChecking.Core
import Nihil.TypeChecking.Substitution
import Nihil.TypeChecking.Constraint
import Nihil.TypeChecking.Common
import Nihil.TypeChecking.Rules.Unification.Kind()
import Nihil.TypeChecking.Environment
import Nihil.TypeChecking.Rules.Solving (runSolve)
import Text.PrettyPrint.ANSI.Leijen (Doc)

-- | Solves the 'KindConstraint's given and returns a substitution to 'apply'.
solve :: Subst Kind -> [KindConstraint] -> SolveKind (Subst Kind)
solve sub []                 = pure sub
solve sub ((k1 :*~ k2) : ks) = do
    sub' <- unify k1 k2
    solve (sub <> sub') (apply sub' ks)

-- | Runs the kind constraint solver given an environment and returns a substitution to 'apply'.
runKindSolver :: KindEnv -> [KindConstraint] -> Either Doc (Subst Kind)
runKindSolver env cons = runSolve env (solve mempty cons)