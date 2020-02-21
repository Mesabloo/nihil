module Nihil.TypeChecking.Rules.Inference where

import Nihil.TypeChecking.Common (Infer, InferState(IState))
import Nihil.TypeChecking.Environment (Env)
import Nihil.TypeChecking.Substitution (Substitutable, free)
import Nihil.TypeChecking.Core (Scheme(..))
import Text.PrettyPrint.ANSI.Leijen (Doc)
import Control.Monad.RWS (evalRWST)
import Control.Monad.Except (runExcept)
import qualified Data.Set as Set

-- | Runs an inference action with a default environment, and return either an error or the result and constraints generated.
runInfer :: e -> Infer e cs a -> Either Doc (a, cs)
runInfer env inf = runExcept (evalRWST inf env (IState 0))

-- | Generalizes a substitutable value with a substitutable environment and returns a 'Scheme' object.
--
--   Here is an example of generalization:
--
--   > f : a -> b
--
--   Assuming @a@ and @b@ aren't rigid, generalizing this type yields
--
--   > f : ∀ a b. a -> b
--
--   which is representated using the 'Scheme' type with:
--
--   > Forall ["a", "b"] (TApplication (TApplication (TId "->") (TVar "a")) (TVar "b"))
generalize :: (Substitutable t, Substitutable a) => Env a -> t -> Scheme t
generalize env t = Forall tvs t
  where tvs = Set.toList (free t `Set.difference` free env)