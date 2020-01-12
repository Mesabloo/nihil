module Nihil.TypeChecking where

import Nihil.TypeChecking.Core (Type, Kind)
import Nihil.TypeChecking.Environment (KindEnv, GlobalEnv)
import Nihil.TypeChecking.Substitution (apply)
import Nihil.TypeChecking.Rules.Inference.Kind (inferKind)
import Nihil.TypeChecking.Rules.Solving.Kind (runKindSolver)
import Nihil.TypeChecking.Rules.Inference (runInfer)
import qualified Nihil.TypeChecking.Rules.Program as RP (typecheck)
import qualified Nihil.Syntax.Abstract.Core as AC (Program)
import Text.PrettyPrint.ANSI.Leijen (Doc)
import Control.Monad.Except (runExcept)
import Control.Monad.State (execStateT)

-- | Runs the kindchecker on a given 'Type', and returns the inferred type.
--
--   Be aware that we should not fully support polymorphics kinds (such as
--   @type T (p :: forall k. [k]) = U p@) for now.
runKindChecker :: KindEnv -> Type -> Either Doc Kind
runKindChecker env ty = do
    (kind, cs) <- runInfer env (inferKind ty)
    sub        <- runKindSolver env cs
    pure (apply sub kind)

-- | Runs the typechecker on a desugared 'AC.Program'. The environment is most likely
--   to be @'GlobalEnv' 'mempty' 'mempty' 'mempty' 'mempty'@ if first run.
--
--   It returns the new 'GlobalEnv' issued after typechecking every definitions.
runTypeChecker :: GlobalEnv -> AC.Program -> Either Doc GlobalEnv
runTypeChecker env prog = runExcept (execStateT (RP.typecheck prog) env)