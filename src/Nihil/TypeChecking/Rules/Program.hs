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
import Control.Lens (use, (%=), uses, (%~))
import qualified Data.Map as Map
import Control.Applicative ((<|>))
import Control.Monad.State (get, put)
import Data.These (These(..))
import Data.Maybe (isJust, fromJust)
import Prelude hiding (lookup, log)
import qualified Prelude (lookup)
import Control.Arrow ((>>>))
import qualified Data.Set as Set
import Data.List (nub, (\\))
import Data.Functor ((<&>))
import Data.Function ((&))

-- | Type checks a whole 'AC.Program'.
typecheck :: AC.Program -> TypeCheck ()
typecheck (AC.Program stts) = do
    defsAndDecls <- separateTypeDefs stts
    statements   <- organizeStatements defsAndDecls
    void (UMap.traverseWithKey handleStatement (uncurry align statements))

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
        handle' cls name (These def decl@(Forall tvs ty)) =
            decl <$ check name def (Just ty)

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

            fun <- (funDefCtx `uses` lookup name) <&> fmap \(Forall _ t) -> t

            (_, cs) <- liftEither (runInfer env (inferFunctionDefinition (location ex) name ex fun))
            log cs (pure ())
            subst <- liftEither (runTypeSolver env cs)

            pure ()
        handle' cls _ name (These ex ty) = do
            env <- get

            fun <- (funDefCtx `uses` lookup name) <&> fmap \(Forall _ t) -> t

            (_, cs) <- liftEither (runInfer env (inferFunctionDefinition (location ex) name ex fun))
            subst <- liftEither (runTypeSolver env (cs & typeConstraint %~ (<> [fromJust fun :>~ coerceType ty])))

            pure ()

        getPos (This e) = location e
        getPos (That d) = location d
        getPos (These e d) = location e

findClassName :: Type -> String
findClassName (annotated -> TId n) = n
findClassName (annotated -> TApplication t1 _) = findClassName t1
findClassName (annotated -> ty) = impossible ("Unfolding class names from type applications cannot take in account " <> show ty)

-------------------------------------------------------------------------------------------------------------------

separateTypeDefs :: [AC.Statement] -> TypeCheck [AC.Statement]
separateTypeDefs []     = pure []
separateTypeDefs ((annotated -> AC.TypeDefinition name tvs cty):ss) = do
    let dummy = Forall tvs ()
    typecheckTypeDef name (Forall tvs (coerceCustomType dummy cty))
    separateTypeDefs ss
separateTypeDefs ((annotated -> AC.ClassDefinition ty stts):ss) = do
    typecheckClassDef (coerceScheme ty) stts
    separateTypeDefs ss
separateTypeDefs ((annotated -> AC.InstanceDefinition ty stts):ss) = do
    typecheckInstance (coerceType ty) stts
    separateTypeDefs ss
separateTypeDefs (d:ss) = (d :) <$> separateTypeDefs ss

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
handleStatement name (This def)       = check name def Nothing
handleStatement name (These def decl) = check name def (Just (coerceType decl))

-- | Type checks a function definition.
check :: String -> AC.Expr -> Maybe Type -> TypeCheck ()
check name def decl = do
    let pos = location def
    env      <- get

    case decl of
        Nothing -> pure ()
        Just t  -> do
            env     <- use typeCtx
            (_, cs) <- liftEither (runInfer env (inferScheme (extractRigids t)))
            _       <- liftEither (runKindSolver env cs)
            pure ()

    (ty, cs) <- liftEither (runInfer env (inferFunctionDefinition pos name def decl))
    sub      <- liftEither (runTypeSolver env cs)
    let funType = apply sub ty
    funDefCtx %= (`extend` (name, closeOver funType))

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
