{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}

module Nihil.TypeChecking.Rules.Program
( typecheck ) where

import Nihil.TypeChecking.Core
import Nihil.TypeChecking.Common
import qualified Nihil.Syntax.Abstract.Core as AC
import Nihil.TypeChecking.Translation.AbstractToCore (coerceCustomType, coerceType)
import Nihil.Utils.Source
import Nihil.Utils.Impossible (impossible)
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
import Data.Align (align)
import qualified Data.Map.Unordered as UMap
import Data.Bifunctor (first)
import Control.Monad.Except (throwError, liftEither)
import Data.Bitraversable (bitraverse, Bitraversable)
import Control.Monad (void, when)
import Control.Lens (use, (%=))
import qualified Data.Map as Map
import Control.Applicative ((<|>))
import Control.Monad.State (get)
import Data.These (These(..))
import Data.Maybe (isJust)
import Prelude hiding (lookup, log)
import Control.Arrow ((>>>))

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
                (get >>= liftEither . flip runTypeSolver [(typeDef :>~ r)])
                    <|> throwError (gadtWrongReturnType name' r typeDef)

            pure ctors
        Forall _ (TypeAlias _)  -> pure mempty

    constructorCtx %= union (Env schemes)

-------------------------------------------------------------------------------------------------------------------

separateTypeDefs :: [AC.Statement] -> TypeCheck [AC.Statement]
separateTypeDefs []     = pure []
separateTypeDefs (d:ss) = case annotated d of
    AC.TypeDefinition name tvs cty -> do
        let dummy = Forall tvs ()
        typecheckTypeDef name (Forall tvs (coerceCustomType dummy cty))
        separateTypeDefs ss
    _                              ->
        (d :) <$> separateTypeDefs ss

-- organizeStatements :: [AC.Statement] -> TypeCheck (UMap.Map String AC.Expr, UMap.Map String AC.Type)
-- organizeStatements []     = pure (mempty, mempty)
-- organizeStatements (s:ss) = do
    -- let fun = case annotated s of
    --         AC.FunctionDeclaration name ty -> secondM (check name ty)
    --           where check name ty umap = case UMap.lookup name umap of
    --                     Nothing -> pure (UMap.insert name ty umap)
    --                     Just _  -> do
    --                         let loc = location ty
    --                         throwError (redeclaredFunction (locate name loc))
    --         AC.FunctionDefinition name ex  -> firstM (check name ex)
    --           where check name ex umap = case UMap.lookup name umap of
    --                     Nothing -> pure (UMap.insert name ex umap)
    --                     Just _  -> do
    --                         let loc = location ex
    --                         throwError (redefinedFunction (locate name loc))
    --         _                              ->
    --             impossible "Type definitions are already filtered!"
--     log s $ fun (organizeStatements ss)

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
    (ty, cs) <- liftEither (runInfer env (inferFunctionDefinition pos name def decl))
    sub      <- liftEither (runTypeSolver env cs)
    liftEither (runTypeHoleInspector sub)
    funDefCtx %= (`extend` (name, generalize (mempty :: TypeEnv) (apply sub ty)))

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