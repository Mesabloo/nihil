{-# LANGUAGE BlockArguments #-}

module Nihil.TypeChecking.Rules.Solving.Type where

import Nihil.TypeChecking.Unification
import Nihil.TypeChecking.Core
import Nihil.TypeChecking.Substitution
import Nihil.TypeChecking.Constraint
import Nihil.TypeChecking.Common
import Nihil.TypeChecking.Rules.Unification.Type()
import Nihil.TypeChecking.Environment
import Nihil.TypeChecking.Rules.Solving (runSolve)
import Nihil.TypeChecking.Errors.TypeHole
import Nihil.Utils.Source
import Nihil.Utils.Debug hiding (error)
import Text.PrettyPrint.ANSI.Leijen (Doc)
import qualified Data.Map as Map
import Control.Monad (when)
import Control.Monad.Except (throwError, liftEither)
import Prelude hiding (log)

-- | Solves the 'TypeConstraint's given and returns a substitution to 'apply'.
solve :: Subst Type -> TCConstraints -> SolveType (Subst Type)
solve sub (TCConstraints [] [])                 = do
    log sub (liftEither (runTypeHoleInspector sub))
    pure sub
solve sub (TCConstraints [] _) = error "Not yet implemented"
solve sub (TCConstraints ((t1 :>~ t2) : ts) ccs) = do
    sub' <- unify t1 t2
    solve (sub' <> sub) (TCConstraints (apply sub' ts) (apply sub' ccs))

-- | Runs the type constraints solver given an initial environment and returns a substitution to 'apply'.
runTypeSolver :: GlobalEnv -> TCConstraints -> Either Doc (Subst Type)
runTypeSolver env cons = runSolve env (solve mempty cons)

-- | Runs the type hole inspector on a substitution to detect type hole variables and throw errors.
runTypeHoleInspector :: Subst Type -> Either Doc ()
runTypeHoleInspector (Subst sub) = do
    let holes = Map.filterWithKey (const . (== '_') . head) sub
    when (not (null holes)) do
        throwError (errs holes)
  where errs = foldMap (typeHole . (`locate` NoSource))