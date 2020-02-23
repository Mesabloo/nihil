{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

module Nihil.TypeChecking.Rules.Program
( typecheck ) where

import Nihil.TypeChecking.Core
import Nihil.TypeChecking.Common
import qualified Nihil.Syntax.Abstract.Core as AC
import Nihil.TypeChecking.Translation.AbstractToCore (coerceCustomType, coerceType, coerceScheme)
import Nihil.Utils.Source
import Nihil.Utils.Debug (info, log)
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
import Nihil.TypeChecking.Errors.NonVisibleMemberOfClass
import Nihil.TypeChecking.Rules.Inference.Type
import Data.Align (align)
import qualified Data.Map.Unordered as UMap
import Data.Bifunctor (first, second)
import Control.Monad.Except (throwError, liftEither)
import Data.Bitraversable (bitraverse, Bitraversable)
import Control.Monad (void, when, replicateM)
import Control.Lens (use, (%=))
import qualified Data.Map as Map
import Control.Applicative ((<|>))
import Control.Monad.State (get, put)
import Data.These (These(..))
import Data.Maybe (isJust, fromJust)
import Prelude hiding (lookup, log)
import qualified Prelude (lookup)
import Control.Arrow ((>>>))
import qualified Data.Set as Set
import Data.List (nub, (\\), partition)
import Data.Functor ((<&>))

-- | Type checks a whole 'AC.Program'.
typecheck :: AC.Program -> TypeCheck ()
typecheck (AC.Program stts) = do
    let (types, funs) = partition isTypeOrClass stts
    toHandle <- uncurry align <$> organizeStatements funs

    void (UMap.traverseWithKey prehandleStatement toHandle)
    mapM handleType types

    void (UMap.traverseWithKey handleStatement toHandle)
  where isTypeOrClass (annotated -> AC.TypeDefinition{}) = True
        isTypeOrClass (annotated -> AC.ClassDefinition{}) = True
        isTypeOrClass (annotated -> AC.InstanceDefinition{}) = True
        isTypeOrClass _ = False

-- | Type checks a custom type definition.
typecheckTypeDef :: String -> Scheme CustomType -> TypeCheck ()
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
                (get >>= liftEither . flip runTypeSolver (TCConstraints [(typeDef :>~ r)] mempty))
                    <|> throwError (gadtWrongReturnType name' r typeDef)

            pure ctors
        Forall _ (TypeAlias _)  -> pure mempty
        Forall _ (Class _ _) -> impossible "Type classes are already filtered"

    constructorCtx %= union (Env schemes)

typecheckClassDef :: Scheme Type -> [AC.Statement] -> TypeCheck ()
typecheckClassDef ty@(Forall tvs t) stts = do
    env <- use typeCtx
    let name = findClassName t

    when (isJust (lookup name env)) do
        throwError (redefinedType (locate name (location t)))

    funs@(_, decls) <- fmap (fmap (extractRigids . coerceType)) <$> organizeStatements stts
    let minusTvs = decls <&> \(Forall tvs' ty) -> Forall (tvs' \\ tvs) ty

    (kind, cs) <- liftEither (runInfer env (inferTypeClass name ty (UMap.elems minusTvs)))
    sub        <- liftEither (runKindSolver env cs)

    typeCtx %= (`extend` (name, apply sub kind))

    let funs' = second (const minusTvs) funs
    tcFuns <- Map.fromList . UMap.toList <$> UMap.traverseWithKey (handle' name) (uncurry align funs')

    customTypeCtx %= (`extend` (name, locate (Forall tvs (Class t tcFuns)) (location t)))
  where handle' :: String -> String -> These AC.Expr (Scheme Type) -> TypeCheck (Scheme Type)
        handle' cls name (This def) = throwError (nonVisibleMemberOf cls name (location def))
        handle' cls name (That decl) = pure decl
        handle' cls name (These def decl@(Forall tvs ty)) = do
            funDefCtx %= (`extend` (name, decl))
            decl <$ check name def

typecheckInstance :: Type -> [AC.Statement] -> TypeCheck ()
typecheckInstance ty stts = do
    let name = findClassName ty
    env <- use customTypeCtx
    let Just (annotated -> Forall _ (Class _ record)) = lookup name env

    funs@(defs, _) <- organizeStatements stts
    instanceCtx %= Map.insert ty (Env (Map.fromList (UMap.toList defs)))

    -- Check that the current instance is valid (that is, a valid typeclass type can be unified to it)
    -- => Kind check the instance's head

    env' <- use typeCtx
    (k, cs) <- liftEither (runInfer env' (inferScheme (extractRigids ty)))
    _ <- liftEither (runKindSolver env' (cs <> [k :*~ KConstraint]))

    -- Generate a substitution between the class' head and the instance's head

    env'' <- get
    (_, cs) <- liftEither (runInfer env'' (inferInstanceHead name ty))
    sub <- liftEither (runTypeSolver env'' cs)

    -- Infer the type of each function definition
    -- => Do not forget to apply the substitution generated above in order to check for function validity
    -- | If the function is not in the class, generate an error because the function is not a visible method of the class.

    env <- get
    let classCtx = apply sub (inferInstanceBody <$> Env record)
    funDefCtx %= union classCtx

    _ <- UMap.traverseWithKey (handle' name record) (uncurry align funs)

    put env
    -- If all the functions are well-formed, add the instance to the instance environment
  where handle' :: String -> Map.Map String (Scheme Type) -> String -> These AC.Expr AC.Type -> TypeCheck ()
        handle' cls record name these
            | name `Map.notMember` record = throwError (nonVisibleMemberOf cls name (getPos these))
        handle' cls _ name (That ty) = throwError (lacksBind (locate name (location ty)))
        handle' cls _ name (This ex) = do
            env <- get

            (_, cs) <- liftEither (runInfer env (inferFunctionDefinition (location ex) name ex))
            log cs (pure ())
            subst <- liftEither (runTypeSolver env cs)

            pure ()
        handle' cls _ name (These ex ty) = do
            env <- get

            (fTy, cs) <- liftEither (runInfer env (inferFunctionDefinition (location ex) name ex))
            subst <- liftEither (runTypeSolver env cs)

            (_, cs) <- liftEither (runInfer env (checkReturnType (coerceType ty) (apply subst fTy)))
            _ <- liftEither (runTypeSolver env cs)

            pure ()

        getPos (This e) = location e
        getPos (That d) = location d
        getPos (These e d) = location e

findClassName :: Type -> String
findClassName (annotated -> TId n) = n
findClassName (annotated -> TApplication t1 _) = findClassName t1
findClassName (annotated -> ty) = impossible ("Unfolding class names from type applications cannot take in account " <> show ty)

-------------------------------------------------------------------------------------------------------------------

handleType :: AC.Statement -> TypeCheck ()
handleType (annotated -> AC.TypeDefinition name tvs cty) = do
    let dummy = Forall tvs ()
    typecheckTypeDef name (Forall tvs (coerceCustomType dummy cty))
handleType (annotated -> AC.ClassDefinition ty stts) = do
    typecheckClassDef (coerceScheme ty) stts
handleType (annotated -> AC.InstanceDefinition ty stts) = do
    typecheckInstance (coerceType ty) stts
handleType _ = impossible "Functions should have been removed!"

organizeStatements :: [AC.Statement] -> TypeCheck (UMap.Map String AC.Expr, UMap.Map String AC.Type)
organizeStatements = go (mempty, mempty)
  where go :: (UMap.Map String AC.Expr, UMap.Map String AC.Type) -> [AC.Statement] -> TypeCheck (UMap.Map String AC.Expr, UMap.Map String AC.Type)
        go res []     = pure res
        go res (s:ss) = fun s res >>= flip go ss

        fun = annotated >>> \case
            AC.FunctionDeclaration name ty -> secondM (check name ty)
                where check name ty umap = case UMap.lookup name umap of
                        Nothing -> pure (UMap.insert name ty umap)
                        Just _  -> do
                            let loc = location ty
                            throwError (redeclaredFunction (locate name loc))
            AC.FunctionDefinition name ex  -> firstM (check name ex)
                where check name ex umap = case UMap.lookup name umap of
                        Nothing -> pure (UMap.insert name ex umap)
                        Just _  -> do
                            let loc = location ex
                            throwError (redefinedFunction (locate name loc))
            _                              ->
                impossible "Type definitions are already filtered!"

handleStatement :: String -> These AC.Expr AC.Type -> TypeCheck ()
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
check :: String -> AC.Expr -> TypeCheck ()
check name def = do
    let pos = location def
    env      <- get

    (ty, cs) <- liftEither (runInfer env (inferFunctionDefinition pos name def))
    sub      <- liftEither (runTypeSolver env cs)
    let funType = apply sub ty

    funDefCtx %= (`extend` (name, closeOver funType))
    funDefCtx %= apply sub -- we apply the substitution to remove types such as %1 which are somewhat placeholders.

extractRigids :: Type -> Scheme Type
extractRigids ty = Forall tvs ty
    where tvs = Set.toList (fold ty)

          fold (annotated -> t) = case t of
              TRigid n -> Set.singleton n
              TApplication t1 t2 -> fold t1 <> fold t2
              TTuple ts -> mconcat (fold <$> ts)
              TImplements t1 t2 -> fold t1 <> fold t2
              _ -> mempty

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
                f (TImplements t1 t2)  = TImplements (rigidify t1) (rigidify t2)
                f t                    = t
