module Nihil.TypeChecking
( runKindChecker, runTypeChecker, defaultGlobalEnv
  -- * Re-exports
, module Nihil.TypeChecking.Pretty
) where

import Nihil.TypeChecking.Core
import Nihil.TypeChecking.Environment (KindEnv, GlobalEnv, Env(..), GlobalEnv(..))
import Nihil.TypeChecking.Substitution (apply)
import Nihil.TypeChecking.Rules.Inference.Kind (inferKind)
import Nihil.TypeChecking.Rules.Solving.Kind (runKindSolver)
import Nihil.TypeChecking.Rules.Inference (runInfer)
import Nihil.Utils.Source (locate, SourcePos(NoSource))
import qualified Nihil.TypeChecking.Rules.Program as RP (typecheck)
import qualified Nihil.Syntax.Abstract.Core as AC (Program)
import Nihil.TypeChecking.Pretty
import Text.PrettyPrint.ANSI.Leijen (Doc)
import Control.Monad.Except (runExcept)
import Control.Monad.State (execStateT)
import qualified Data.Map as Map

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

-- | The minimal default environment for typechecking (contains built-in types and functions).
defaultGlobalEnv :: GlobalEnv
defaultGlobalEnv = GlobalEnv (Env defaultTypeCtx) (Env defaultCustomTypes) (Env defaultFunCtx) (Env defaultConstructors)
  where dummyPos = NoSource

        tFun t1 t2 = locate (TApplication (locate (TApplication (locate (TId "->") dummyPos) t1) dummyPos) t2) dummyPos
        kArr k1 k2 = KApplication (KApplication KArrow k1) k2

        defaultTypeCtx :: Map.Map String Kind
        defaultTypeCtx = Map.fromList
            [ ("Integer", KStar)
            , ("Double",  KStar)
            , ("Char",    KStar)
            , ("List",    KStar `kArr` KStar)
            , ("()",      KStar) ]

        defaultCustomTypes :: Map.Map String CustomType
        defaultCustomTypes = Map.fromList
            [ ("List", locate (
                    Forall ["a"] (GADT (Map.fromList
                            [ ("Nil",  defaultConstructors Map.! "Nil")
                            , ("Cons", defaultConstructors Map.! "Cons") ]
                                 ))
                       ) dummyPos)
            , ("()",   locate (
                    Forall [] (GADT (Map.fromList
                            [ ("()", defaultConstructors Map.! "()")]
                              ))
                       ) dummyPos)
            ]

        defaultConstructors :: Map.Map String (Scheme Type)
        defaultConstructors = Map.fromList
            [ ("Nil",  Forall ["a"] (locate (TApplication (locate (TId "List") dummyPos) (locate (TVar "a") dummyPos)) dummyPos))
                -- Nil : ∀ a. List a
            , ("Cons", Forall ["a"] (locate (TVar "a") dummyPos `tFun` locate (TApplication (locate (TId "List") dummyPos) (locate (TVar "a") dummyPos)) dummyPos))
                -- Cons : ∀ a. a → List a
            , ("()",   Forall [] (locate (TId "()") dummyPos))
                -- () : ()
            ]

        defaultFunCtx :: Map.Map String (Scheme Type)
        defaultFunCtx = Map.fromList
            [ ("+", Forall ["a"] (locate (TVar "a") dummyPos `tFun` (locate (TVar "a") dummyPos `tFun` locate (TVar "a") dummyPos)))
                -- (+) : ∀ a. a → a → a
            , ("-", Forall ["a"] (locate (TVar "a") dummyPos `tFun` (locate (TVar "a") dummyPos `tFun` locate (TVar "a") dummyPos)))
                -- (-) : ∀ a. a → a → a
            , ("/", Forall ["a"] (locate (TVar "a") dummyPos `tFun` (locate (TVar "a") dummyPos `tFun` locate (TVar "a") dummyPos)))
                -- (/) : ∀ a. a → a → a
            , ("*", Forall ["a"] (locate (TVar "a") dummyPos `tFun` (locate (TVar "a") dummyPos `tFun` locate (TVar "a") dummyPos)))
                -- (*) : ∀ a. a → a → a
            ]