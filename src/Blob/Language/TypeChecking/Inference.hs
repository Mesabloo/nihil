-- Blobc, a compiler for compiling Blob source code
-- Copyright (c) 2019 Mesabloo

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE LambdaCase, BlockArguments, TupleSections #-}

-- | This module holds all the type checking functions.
module Blob.Language.TypeChecking.Inference where

import Blob.Language.TypeChecking.Types
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Set as Set
import Blob.Language.TypeChecking.Errors
import qualified Data.Map as Map
import Blob.Language.Desugaring.Types hiding (Type(..), Scheme, CustomType(..))
import qualified Blob.Language.Desugaring.Types as TP (Type(..), Scheme(..), CustomType(..))
import Control.Monad.RWS
import Control.Monad.Identity
import Data.List (nub)
import Text.PrettyPrint.Leijen (text, dot, linebreak, empty)
import Control.Monad.Reader
import Control.Applicative ((<|>), liftA2)
import Data.Bifunctor (first, second, bimap)
import Data.These
import qualified Data.Map.Unordered as UMap
import Blob.Language.KindChecking.Checker
import Blob.Language.KindChecking.Types
import Data.Align.Key (alignWithKey)
import Blob.Language.Parsing.Annotation
import Data.Maybe
import Control.Lens
import Data.Composition ((.:))
import Prelude hiding (lookup)
import qualified Prelude (lookup)
import Debug.Trace

-- | Runs the inference monad given as argument.
runInfer :: GlobalEnv -> Infer (Type, [Constraint]) -> Either TIError ((Type, [Constraint]), [Constraint])
runInfer env m = runIdentity . runExceptT $ evalRWST m env initInfer
  where initInfer = InferState 0

-- | Solves the type for a given 'Expr' in a given 'GlobalEnv'.
inferExpr :: GlobalEnv -> Annotated Expr -> Either TIError Scheme
inferExpr env ex = do
    (ty, c) <- fst <$> runInfer env (infer ex)
    subst <- runSolve env c
    runHoleInspect subst
    pure . closeOver $ apply subst ty

-- | Infers the definition of a function given a 'GlobalEnv', the name of the function and its value as an 'Expr'.
inferDef :: GlobalEnv -> String -> Annotated Expr -> Maybe Type -> Check ()
inferDef env name def t1 =
    let res = runInfer env $
            do var <- fresh "#"
               (t, c) <- inEnv (name, Scheme [] var) $ infer def
               pure (t, c <> [(t, var)])
    in case res of
        Left err -> throwError err
        Right ((t,c),_) -> case runSolve env ((if isNothing t1 then ([] <>) else ((t, fromJust t1) :)) c) of
            Left err -> throwError err
            Right x -> case runHoleInspect x of
                Left err -> throwError err
                Right _ ->
                    defCtx %= ((TypeEnv $ Map.singleton name (closeOver (apply x t))) `union`)


-- | Returns the internal constraints used in solving for the type of an expression.
constraintsExpr :: GlobalEnv -> Annotated Expr -> Either TIError ([Constraint], Subst, Type, Scheme)
constraintsExpr env ex = do
    (ty, c) <- fst <$> runInfer env (infer ex)
    subst <- runSolve env c
    pure (c, subst, ty, sc subst ty)
  where sc = closeOver .: apply

-- | Canonicalizes and returns the polymorphic toplevel type.
closeOver :: Type -> Scheme
closeOver = normalize . generalize mempty

-- | Extends the type environment with a single entry.
inEnv :: (String, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m =
    flip local m $ do
        env <- ask
        let ctx = env ^. defCtx
        pure $ env & defCtx .~ (remove ctx x `extend` (x, sc) `union` ctx)

-- | Extends the type environment with multiple entries.
inEnvMany :: [(String, Scheme)] -> Infer a -> Infer a
inEnvMany list m = do
    let env = TypeEnv $ Map.fromList list
    local (defCtx %~ (env `union`)) m

-- | Returns the type of a constant/function from the environment.
lookupEnv :: String -> Infer Type
lookupEnv x = do
    env <- defCtx `views` lookup x
    env' <- ctorCtx `views` lookup x
    maybe (throwError $ makeUnboundVarError x) instantiate (env <|> env')

-- | Unwraps the underlying 'Map.Map' from a 'TypeEnv'.
getMap :: TypeEnv -> Map.Map String Scheme
getMap = (^. _TypeEnv)
{-# INLINE getMap #-}

-- | Extends a given 'TypeEnv' with a new function.
extend :: TypeEnv -> (String, Scheme) -> TypeEnv
extend = TypeEnv .: flip (uncurry Map.insert) . getMap

-- | The empty substitution.
nullSubst :: Subst
nullSubst = mempty

-- | A special way of composing substitutions together.
compose :: Subst -> Subst -> Subst
compose s1 s2 = Map.map (apply s1) s2 `Map.union` s1

-- | The empty unifier (no substitution, no constraint).
emptyUnifier :: Unifier
emptyUnifier = (nullSubst, [])

-- | Removes an entry from the 'TypeEnv' given.
remove :: TypeEnv -> String -> TypeEnv
remove = TypeEnv .: flip Map.delete . getMap

-- | Merges two 'TypeEnv's together.
union :: TypeEnv -> TypeEnv -> TypeEnv
union t1 t2 = TypeEnv (getMap t1 `Map.union` getMap t2)

-- | Lookup into a 'TypeEnv'.
lookup :: String -> TypeEnv -> Maybe Scheme
lookup k t1 = Map.lookup k (getMap t1)

-- | Creates a new type variable with a given prefix.
fresh :: String -> Infer Type
fresh v = do
    s <- use count
    count += 1

    env <- defCtx `views` \t -> concat (fst . (^. _Scheme) <$> Map.elems (getMap t))
    let newTVar = TV (v <> show s)

    if newTVar `notElem` env
    then pure (TVar newTVar)
    else fresh (newTVar ^. _TV)

-- | Instantiate a 'Scheme' to produce a fresh 'Type'.
instantiate :: Scheme -> Infer Type
instantiate (Scheme as t) = do
    as' <- mapM (const $ fresh "@") as
    let s = Map.fromList $ zip as as'
    pure $ apply s (relax t)

-- | Creates a 'Scheme' from a 'Type' by putting all the free type variables from the 'Type' into the 'Scheme'.
generalize :: TypeEnv -> Type -> Scheme
generalize env t  = Scheme as t
    where as = Set.toList $ ftv t `Set.difference` ftv env

-- | Transforms all the free type variables into rigid type variables in a given 'Type'.
rigidify :: Type -> Type
rigidify t = let subst = Map.fromList . Set.toList $ Set.map (ap (,) TRigid) (ftv t)
             in apply subst t

-- | Transforms all the rigid type variables into free type variables in a given 'Type'.
relax :: Type -> Type
relax (TRigid n)    = TVar n
relax (TFun t1 t2)  = TFun (first relax t1) (relax t2)
relax (TTuple ts)   = TTuple (map relax ts)
relax (TApp t1 t2)  = TApp (relax t1) (relax t2)
relax t = t

-- | Infers the 'Type' and 'Constraint's for a given 'Expr'ession.
infer :: Annotated Expr -> Infer (Type, [Constraint])
infer (e :- _) = case e of
    ELit (LInt _) -> pure (TInt, [])
    ELit (LDec _) -> pure (TFloat, [])
    ELit (LChr _) -> pure (TChar, [])
    EHole -> do
        tv <- fresh "_"
        tv' <- fresh "#"
        pure (tv, [(tv, tv')])
    EId x ->
        (, []) <$> lookupEnv x
    ELam x e' -> do
        (pat, cs, env) <- inferPattern x

        (t, c) <- inEnvMany (Map.toList env) (infer e')

        pure ((pat, 0) `TFun` t, cs <> c)
    EApp e1 e2 -> do
        (t1, c1) <- infer e1
        (t2, c2) <- infer e2
        tv <- fresh "#"
        pure (tv, c1 <> c2 <> [(t1, (t2, 1) `TFun` tv)])
    ETuple es -> do
        ts <- mapM infer es
        pure (TTuple $ map fst ts, foldMap snd ts)
    EAnn e t -> do
        (t', c) <- infer e
        pure (t', (tiType t, t') : c)
    ELet stts e -> do
        let inferStatement name = \case
                This def ->
                    do  var <- fresh "&"
                        (t, c) <- inEnv (name, Scheme [] var) $ infer def
                        pure (Scheme [] t, c <> [(t, var)])
                That _ -> throwError $ makeBindLackError name
                These def decl ->
                    do  var <- fresh "&"
                        (t, c) <- inEnv (name, Scheme [] var) $ infer def
                        (s, cs) <- pure (Scheme [] t, c <> [(t, var)])
                        pure (s, (t, tiType decl) : cs)

            sepStatements' [] = ([], [])
            sepStatements' ((Definition name def :- _):xs) = first ((name, def) :) $ sepStatements' xs
            sepStatements' ((Declaration name decl :- _):xs) = second ((name, decl) :) $ sepStatements' xs
            sepStatements' (_:xs) = sepStatements' xs
        map' <- sequence $ uncurry (alignWithKey inferStatement)
                    (bimap Map.fromList Map.fromList $ sepStatements' stts)

        let env = Map.map fst map'
            cs = concat . Map.elems $ Map.map snd map'

        (t3, c3) <- inEnvMany (Map.toList env) (infer e)
        pure (t3, cs <> c3)
    EMatch e cases -> do
        (tExp, tCon) <- infer e

        res <- (unzip3 <$>) . forM cases $ \(pat, expr) -> do
            (patTy, patsCons, env) <- inferPattern pat

            (exprTy, exprCons) <- inEnvMany (Map.toList env) (infer expr)
            pure (exprTy, patTy, exprCons <> patsCons)

        let (ret:xs, patsTy, pCons) = res
            types = zipFrom ret xs <> zipFrom tExp patsTy
            cons = mconcat pCons

        pure (ret, types <> cons <> tCon)
      where
        zipFrom :: a -> [b] -> [(a, b)]
        zipFrom = zip . repeat

-- | Infers the 'Type', the 'Constraint's and a mapping for the types of each pattern variale from a 'Pattern'.
inferPattern :: Annotated Pattern -> Infer (Type, [Constraint], Map.Map String Scheme)
inferPattern (p :- _) = case p of
    Wildcard -> do
        t <- fresh "&"
        pure (t, [], mempty)
    PInt _ -> pure (TInt, [], mempty)
    PDec _ -> pure (TFloat, [], mempty)
    PChr _ -> pure (TChar, [], mempty)
    PId id' -> do
        t <- fresh "&"
        pure (t, [], Map.singleton id' (Scheme [] t))
    PTuple exp -> do
        pats <- mapM inferPattern exp
        let (ts, cs, envs) = unzip3 pats
        pure (TTuple ts, mconcat cs, mconcat envs)
    PAnn p t -> do
        (t', cs, env) <- inferPattern p
        let t'' = tiType t
        pure (t'', (t'', t') : cs, env)
    PCtor id' args -> do
        ctor <- instantiate =<< lookupCtor id'
        let (ts, r) = unfoldParams ctor

        guard (length args == length ts)
            <|> throwError (text "Expected " <> text (show $ length ts) <> text " arguments to constructor \"" <> text id' <> text "\", but got " <> text (show $ length args) <> dot <> linebreak)

        (ts', cons, env) <- fmap mconcat <$> mapAndUnzip3M inferPattern args

        let cons' = zip (fst <$> ts) ts'

        pure (r, cons' <> mconcat cons, env)
  where
    -- | Returns the 'Scheme' of a constructor.
    lookupCtor :: String -> Infer Scheme
    lookupCtor id' = do
        env <- ctorCtx `views` (Map.lookup id' . getMap)
        case env of
            Nothing -> throwError $ makeUnboundVarError id'
            Just x  -> pure x

    -- | mapAndUnzipM for triples
    mapAndUnzip3M :: Monad m => (a -> m (b, c, d)) -> [a] -> m ([b], [c], [d])
    mapAndUnzip3M _ []     = return ([],[],[])
    mapAndUnzip3M f (x:xs) = do
        (r1,  r2,  r3)  <- f x
        (rs1, rs2, rs3) <- mapAndUnzip3M f xs
        return (r1:rs1, r2:rs2, r3:rs3)

-- | An infinite stream of letters, looping on the alphabet.
letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

-- | Gives fresh new rigid type variables to a 'Scheme'.
normalize :: Scheme -> Scheme
normalize (Scheme _ body) = Scheme (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (map TV letters)

    fv (TVar a)    = [a]
    fv (TFun a b)  = fv (fst a) <> fv b
    fv (TApp a b)  = fv a <> fv b
    fv (TTuple e)  = foldMap fv e
    fv _           = []

    normtype (TFun a b)       = TFun (first normtype a) (normtype b)
    normtype (TApp a b)       = TApp (normtype a) (normtype b)
    normtype (TTuple e)       = TTuple (map normtype e)
    normtype (TVar a@(TV x')) =
        case Prelude.lookup a ord of
            Just x -> TRigid x
            Nothing -> error $ "The type variable \"" <> x' <> "\" has not been declared in type, but wants to be used.\n"
                                <> "This error should never happen. If you see it, please report it."
    normtype t          = t

-- | Runs the constraint solver
runSolve :: GlobalEnv -> [Constraint] -> Either TIError Subst
runSolve g cs = runReader (runExceptT $ solver st) g
  where st = (nullSubst, cs)

-- | Unifies many types into a single substitution.
unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany []         []         = pure nullSubst
unifyMany (t1 : ts1) (t2 : ts2) = do
    su1 <- unifies t1 t2
    su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
    pure (su2 `compose` su1)
unifyMany t1 t2 = throwError $ foldl (\acc (t1', t2') -> acc <> makeUnifyError t1' t2') empty (zip t1 t2)

-- | Unifies two types into a single substitution.
unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2          = pure nullSubst
unifies (TVar v) t                = v `bind` t
unifies t            (TVar v    ) = v `bind` t
unifies (TId i) t       = unifyAlias i t
unifies t       (TId i) = unifyAlias i t
unifies (TFun (t1, l1) t2) (TFun (t3, l2) t4) = unifyMany [t1, t2] [t3, t4]
unifies (TTuple e) (TTuple e')    = unifyMany e e'
unifies (TApp t1 t2) (TApp t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies a@(TApp _ _) t = unifyCustom a t
unifies t a@(TApp _ _) = unifyCustom a t
unifies t1           t2           =
    let (Scheme _ st1) = closeOver t1
        (Scheme _ st2) = closeOver t2
    in throwError $ makeUnifyError st1 st2

-- | Unifies a type application, searching in the environment for custom types.
unifyCustom :: Type -> Type -> Solve Subst
unifyCustom a@(TApp t1 t2) t3 = go t1 [t2]
  where go (TApp t1' t2') args = go t1' (t2':args)
        go (TId i) args = typeDefCtx `views` Map.lookup i >>= \case
            Just (CustomScheme tvs (TAlias t)) ->
                let sub = Map.fromList (zipWith (\k v -> (TV k, v)) tvs args)
                in unifies (apply sub t) t3
            Just _ ->
                let (Scheme _ st3) = closeOver t3
                in throwError $ makeUnifyError a st3
            Nothing -> undefined -- ? case handled by kind checking
        go _ _ = let (Scheme _ st3) = closeOver t3
                 in throwError $ makeUnifyError a st3
unifyCustom _ _ = undefined -- ! never happening

-- | Unifies a type with a type alias, searching in the environment.
unifyAlias :: String -> Type -> Solve Subst
unifyAlias name t1 =
    typeDefCtx `views` Map.lookup name >>= \case
        Just (CustomScheme _ (TAlias t2)) -> unifies t2 t1
        Just _ ->
            let (Scheme _ st1) = closeOver t1
            in throwError $ makeUnifyError (TId name) st1
        Nothing ->
            let (Scheme _ st1) = closeOver t1
            in throwError $ makeUnifyError (TId name) st1 -- ? Should never happen

-- The core algorithm of the unification solver.
solver :: Unifier -> Solve Subst
solver (su, cs) = case cs of
    []               -> pure su
    ((t1, t2) : cs0) -> do
        su1 <- unifies t1 t2
        solver (su1 `compose` su, apply su1 cs0)

-- | Binds a type variable to a type, checking for infinite types.
bind :: TVar -> Type -> Solve Subst
bind a t | t == TVar a     = case a of
                        TV (h:_) | h == '_' ->
                            let (Scheme _ st) = closeOver t
                            in throwError $ makeHoleError st
                        _ -> pure nullSubst
         | occursCheck a t = throwError $ makeOccurError a t
         | otherwise       = pure $ Map.singleton a t

-- | Checks whether we formed an infinite type of not.
occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

-- | Runs the type hole solver.
runHoleInspect :: Subst -> Either TIError ()
runHoleInspect subst =
    let map' = Map.filterWithKey (\(TV k) _ -> head k == '_') subst
    in if null map'
        then pure ()
        else throwError $ Map.foldl (\acc t -> acc <> let (Scheme _ st) = closeOver t in makeHoleError st) empty map'

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

-- | Transforms a 'TP.Type' into a 'Type', for data type compatibility.
tiType :: Annotated TP.Type -> Type
tiType (TP.TId id' :- _)        = TId id'
tiType (TP.TFun t1 t2 :- _)     = TFun (first tiType t1) (tiType t2)
tiType (TP.TTuple ts :- _)      = TTuple (map tiType ts)
tiType (TP.TApp t1 t2 :- _)     = TApp (tiType t1) (tiType t2)
tiType (TP.TVar id' :- _)       = TVar (TV id')

-- | Transforms a 'TP.Scheme' into a 'Scheme', also for compatibility reasons.
tiScheme :: TP.Scheme -> Scheme
tiScheme (TP.Scheme tvs t) = Scheme (map TV tvs) (tiType t)

-- | Transforms a 'TP.CustomType' into a 'CustomType', also for compatibility reasons.
tiCustomType :: Annotated TP.CustomType -> CustomType
tiCustomType (TP.TSum cs :- _)   = TSum (fmap tiScheme cs)
tiCustomType (TP.TAlias t :- _)  = TAlias (tiType t)

-- | Separate statements depending on whether they are a function definition or function declaration.
sepStatements :: [Statement] -> Check (UMap.Map String (Annotated Expr), UMap.Map String (Annotated TP.Type))
sepStatements = uncurry (liftA2 (,)) . bimap (foldDecls makeRedefinedError mempty) (foldDecls makeRedeclaredError mempty) . separateDecls
  where
    separateDecls []                              = mempty
    separateDecls (Declaration id' type' : stmts) = second ((id', type'):) (separateDecls stmts)
    separateDecls (Definition id' expr : stmts)   = first ((id', expr):) (separateDecls stmts)
    separateDecls (_ : stmts)                     = separateDecls stmts

    foldDecls :: (String -> TIError) -> UMap.Map String a -> [(String, a)] -> Check (UMap.Map String a)
    foldDecls _ m []              = pure m
    foldDecls err m ((id', t):ts) = case UMap.lookup id' m of
                                        Nothing -> flip (foldDecls err) ts $ UMap.insert id' t m
                                        Just _ -> throwError $ err id'

-- | Type checks a function definition, a function declaration (error) or both.
handleStatement :: String -> These (Annotated Expr) (Annotated TP.Type) -> Check ()
handleStatement name (This def)      = do
    e <- get
    inferDef e name def Nothing
handleStatement name (That _)        = throwError $ makeBindLackError name
handleStatement name (These def typ) = do
    let ti     = tiType typ
    let gen'ed = closeOver (relax ti)
    checkKI $ kiScheme gen'ed

    e <- get
    inferDef e name def (Just ti)

-- | Kind checks a type declaration.
analyseTypeDecl :: String -> CustomScheme -> Check ()
analyseTypeDecl k v = do
    kind <- checkKI $ do
        var        <- newKindVar "r"
        (subst, t) <- local (Map.insert k var) (kiCustomScheme v)
        subst1     <- mguKind (applyKind subst var) (applyKind subst t)
        pure $ applyKind (subst1 `composeKindSubst` subst) var

    let (CustomScheme tvs t) = v
    schemes <- case t of
        TSum ctors -> do
            let typeDef = foldl (\acc t -> acc `TApp` TVar (TV t)) (TId k) tvs

            void . flip Map.traverseWithKey ctors $ \ctorName (Scheme _ c) -> do
                let (_, r) = unfoldParams c

                e <- get

                case runSolve e [(typeDef, r)] of
                    Left _ -> throwError $ makeGADTWrongReturnTypeError ctorName r typeDef
                    Right _ -> pure ()

            pure ctors
        _          -> pure $ Map.fromList []

    typeDefCtx %= Map.insert k v
    typeDeclCtx %= Map.insert k kind
    ctorCtx %= (TypeEnv schemes `union`)

-- | Unfold the parameters and the return type from a type.
unfoldParams :: Type -> ([(Type, Integer)], Type)
unfoldParams (TFun a b) = first (a:) (unfoldParams b)
unfoldParams t = ([], t)

-- | type checks a whole 'Program'.
tiProgram :: Annotated Program -> Check ()
tiProgram (Program stmts :- _) = do
    remaining <- sepTypeDecls stmts
    stts      <- sepStatements remaining
    sequence_ $ uncurry (alignWithKey handleStatement) stts
  where sepTypeDecls [] = pure []
        sepTypeDecls ((TypeDeclaration k tvs t :- _):ss) = do
            analyseTypeDecl k (CustomScheme tvs (tiCustomType t))
            sepTypeDecls ss
        sepTypeDecls ((s :- _):ss) = (s:) <$> sepTypeDecls ss

-- Runs the type inference given a 'GlobalEnv' and an action.
runTypeInference :: GlobalEnv -> Check a -> Either TIError (a, GlobalEnv)
runTypeInference g p = runExcept (runStateT p g)