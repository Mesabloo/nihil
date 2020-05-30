{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

module Nihil.TypeChecking.Rules.Program
( typecheck ) where

import Nihil.TypeChecking.Core
import Nihil.TypeChecking.Common
import qualified Nihil.Syntax.Abstract.Core as AC
import Nihil.TypeChecking.Translation.AbstractToCore (coerceCustomType, coerceType)
import Nihil.Utils.Source
import Nihil.Utils.Impossible (impossible)
import Nihil.Utils.Annotation (hoistAnnotated)
import Nihil.TypeChecking.Errors.RedeclaredFunction
import Nihil.TypeChecking.Errors.Redefined
import Nihil.TypeChecking.Rules.Inference
import Nihil.TypeChecking.Rules.Inference.Kind
import Nihil.TypeChecking.Rules.Solving.Kind
import Nihil.TypeChecking.Substitution
import Nihil.TypeChecking.Environment
import Nihil.TypeChecking.Constraint
import Nihil.TypeChecking.Rules.Solving.Type
import Nihil.TypeChecking.Errors.GADTWrongReturnType
import Nihil.TypeChecking.Errors.BindLack
import Nihil.TypeChecking.Rules.Inference.Type
import qualified Nihil.Runtime.Core as RC
import Data.Align (align)
import Data.Bifunctor (first, second)
import Control.Monad.Except (throwError, liftEither)
import Data.Bitraversable (bitraverse, Bitraversable)
import Control.Monad (void, when, replicateM)
import Control.Lens (use, (%=))
import qualified Data.Map as Map
import Control.Applicative ((<|>))
import Control.Monad.State (get)
import Data.These (These(..))
import Data.Maybe (isJust, fromJust)
import Prelude hiding (lookup, log)
import qualified Prelude (lookup)
import Control.Arrow ((>>>))
import qualified Data.Set as Set
import Data.List (nub)

-- | Type checks a whole 'AC.Program'.
typecheck :: AC.Program -> TypeCheck ([(String, RC.VExpr)], [String])
typecheck (AC.Program stts) = do
    (defsAndDecls, cons) <- separateTypeDefs stts
    statements   <- organizeStatements defsAndDecls

    let toHandle = uncurry align statements

    void (Map.traverseWithKey prehandleStatement toHandle)
    stts <- Map.toList <$> Map.traverseWithKey handleStatement toHandle

    pure (stts, cons)

-- | Type checks a custom type definition.
typecheckTypeDef :: String -> Scheme CustomType -> TypeCheck [String]
typecheckTypeDef name sc@(Forall tvs cty) = do
    env        <- use typeCtx

    when (isJust (lookup name env)) do
        throwError (redefinedType (locate name (location cty)))

    (kind, cs) <- liftEither (runInfer env (inferCustomType (location cty) name sc))
    sub        <- liftEither (runKindSolver env cs)

    customTypeCtx  %= (`extend` (name, cty))
    typeCtx        %= (`extend` (name, apply sub kind))

    schemes    <- case annotated cty of
        Forall tvs (GADT ctors) -> do
            let pos     = location cty
            let typeDef = foldl ((. TVar) . tApp pos) (locate (TId name) pos) tvs

            flip Map.traverseWithKey ctors \name' (Forall _ ty) -> do
                let (_, r) = foldParams ty
                (get >>= liftEither . flip runTypeSolver [(typeDef :>~ r)])
                    <|> throwError (gadtWrongReturnType name' r typeDef)

            pure ctors
        Forall _ (TypeAlias _)  -> pure mempty

    constructorCtx %= union (Env schemes)
    pure (Map.keys schemes)

-------------------------------------------------------------------------------------------------------------------

separateTypeDefs :: [AC.Statement] -> TypeCheck ([AC.Statement], [String])
separateTypeDefs []     = pure ([], [])
separateTypeDefs (d:ss) = case annotated d of
    AC.TypeDefinition name tvs cty -> do
        let dummy = Forall tvs ()
        cons <- typecheckTypeDef name (Forall tvs (coerceCustomType dummy cty))
        second (mappend cons) <$> separateTypeDefs ss
    _                              ->
        first (d :) <$> separateTypeDefs ss

organizeStatements :: [AC.Statement] -> TypeCheck (Map.Map String AC.Expr, Map.Map String AC.Type)
organizeStatements = go (mempty, mempty)
  where go :: (Map.Map String AC.Expr, Map.Map String AC.Type) -> [AC.Statement] -> TypeCheck (Map.Map String AC.Expr, Map.Map String AC.Type)
        go res []     = pure res
        go res (s:ss) = fun s res >>= flip go ss

        fun = annotated >>> \case
            AC.FunctionDeclaration name ty -> secondM (check name ty)
                where check name ty umap = case Map.lookup name umap of
                        Nothing -> pure (Map.insert name ty umap)
                        Just _  -> do
                            let loc = location ty
                            throwError (redeclaredFunction (locate name loc))
            AC.FunctionDefinition name ex  -> firstM (check name ex)
                where check name ex umap = case Map.lookup name umap of
                        Nothing -> pure (Map.insert name ex umap)
                        Just _  -> do
                            let loc = location ex
                            throwError (redefinedFunction (locate name loc))
            _                              ->
                impossible "Type definitions are already filtered!"

handleStatement :: String -> These AC.Expr AC.Type -> TypeCheck RC.VExpr
handleStatement name (That ty)        = throwError (lacksBind (locate name (location ty)))
handleStatement name (This def)       = check name def
handleStatement name (These def decl) = check name def

prehandleStatement :: String -> These AC.Expr AC.Type -> TypeCheck ()
prehandleStatement name (That ty)        = throwError (lacksBind (locate name (location ty)))
prehandleStatement name (This def)       = do
    env <- get
    (ty, _) <- liftEither (runInfer env (fresh "%" (location def)))
    funDefCtx %= (`extend` (name, Forall [] ty))
prehandleStatement name (These def decl) = do
    let ty = extractRigids (coerceType decl)
    env <- use typeCtx
    (k, cs) <- liftEither (runInfer env (inferScheme ty))
    _ <- liftEither (runKindSolver env cs)
    funDefCtx %= (`extend` (name, ty))

-- | Type checks a function definition.
check :: String -> AC.Expr -> TypeCheck RC.VExpr
check name def = do
    let pos = location def
    env      <- get

    ((ex, ty), cs) <- liftEither (runInfer env (elabFunctionDefinition pos name def))
    sub            <- liftEither (runTypeSolver env cs)
    let funType = apply sub ty

    funDefCtx %= (`extend` (name, closeOver funType))
    funDefCtx %= apply sub -- we apply the substitution to remove types such as %1 which are somewhat placeholders.

    pure ex

extractRigids :: Type -> Scheme Type
extractRigids ty = Forall tvs ty
    where tvs = fold ty

          fold (annotated -> t) = case t of
              TRigid n -> [n]
              TApplication t1 t2 -> fold t1 <> fold t2
              TTuple ts -> concatMap fold ts
              _ -> []

-------------------------------------------------------------------------------------------------------------------

-- | Monadic 'Data.Bifunctor.second'
secondM :: (Bitraversable p, Monad m) => (b -> m d) -> p c b -> m (p c d)
secondM = bitraverse pure

-- | Monadic 'Data.Bifunctor.first'
firstM :: (Bitraversable p, Monad m) => (a -> m c) -> p a b -> m (p c b)
firstM = flip bitraverse pure

tApp :: SourcePos -> Type -> Type' -> Type
tApp pos t1 t2 = locate (TApplication t1 (locate t2 pos)) pos

foldParams :: Type -> ([Type], Type)
foldParams t = case annotated t of
    TApplication t1 t2 -> case annotated t1 of
        TApplication t3 t4
          | annotated t3 == TId "->" -> first (t4 :) (foldParams t2)
          | annotated t3 == TId "→"  -> first (t4 :) (foldParams t2)
        _                            -> ([], t)
    _                  -> ([], t)

closeOver :: Type -> Scheme Type
closeOver = normalize . generalize (mempty :: TypeEnv)

normalize :: Scheme Type -> Scheme Type
normalize (Forall _ t) = Forall (snd <$> ord) (rigidify t)
  where ord = zip (nub (Set.toList (free t))) letters

        letters = [1..] >>= flip replicateM ['a'..'z']

        rigidify :: Type -> Type
        rigidify = hoistAnnotated (first f)
          where f (TApplication t1 t2) = TApplication (rigidify t1) (rigidify t2)
                f (TTuple ts)          = TTuple (rigidify <$> ts)
                f (TVar x)             = TRigid (fromJust (Prelude.lookup x ord))
                f t                    = t
