module Nihil.TypeChecking.Rules.Solving where

import Nihil.TypeChecking.Common
import Text.PrettyPrint.ANSI.Leijen
import Control.Monad.Reader (runReader)
import Control.Monad.Except (runExceptT)

-- | Runs a constraint solving action with a default environment and returns either an error or its result.
runSolve :: e -> Solve e a -> Either Doc a
runSolve env solve = runReader (runExceptT solve) env