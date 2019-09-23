{-# LANGUAGE LambdaCase, BlockArguments, TupleSections #-}

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
import MonadUtils (mapAndUnzip3M)
import Control.Applicative ((<|>), liftA2)
import Data.Bifunctor (first, second, bimap)
import Data.These
import qualified Data.Map.Unordered as UMap
import Blob.Language.KindChecking.Checker
import Blob.Language.KindChecking.Types
import Data.Align.Key (alignWithKey)
import Blob.Language.Parsing.Annotation
import Data.Functor.Invariant (invmap)
import Debug.Trace
import Data.Functor (($>))

-- | Run the inference monad
runInfer :: GlobalEnv -> Infer (Type, [Constraint]) -> Either TIError ((Type, [Constraint]), [Constraint])
runInfer env m = runIdentity . runExceptT $ evalRWST m env initInfer
  where initInfer = InferState { count = 0, linearities = mempty }

-- | Solve for the toplevel type of an expression in a given environment
inferExpr :: GlobalEnv -> Annotated Expr -> Either TIError Scheme
inferExpr env ex = do
    (ty, c) <- fst <$> runInfer env (infer ex)
    subst <- runSolve env c
    runHoleInspect subst
    pure . closeOver $ apply subst ty

inferDef :: GlobalEnv -> String -> Annotated Expr -> Check ()
inferDef env name def =
    let res = runInfer env $
            do var <- fresh "#"
               (t, c) <- inEnv (name, Scheme [] var) $ infer def
               pure (t, c <> [(t, var)])
    in case res of
        Left err -> throwError err
        Right ((t,c),_) -> case runSolve env c of
            Left err -> throwError err
            Right x -> case runHoleInspect x of
                Left err -> throwError err
                Right _ -> modify $ \st -> st
                    { defCtx = let env' = defCtx st
                                   tv = Map.singleton name (closeOver (apply x t)) `Map.union` getMap env'
                               in TypeEnv tv }


-- | Return the internal constraints used in solving for the type of an expression
constraintsExpr :: GlobalEnv -> Annotated Expr -> Either TIError ([Constraint], Subst, Type, Scheme)
constraintsExpr env ex = case runInfer env (infer ex) of
    Left err -> Left err
    Right ((ty, c), _) -> case runSolve env c of
        Left err -> Left err
        Right subst -> Right (c, subst, ty, sc)
          where sc = closeOver $ apply subst ty

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> Scheme
closeOver = normalize . generalize mempty

-- | Extend type environment
inEnv :: (String, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m =
    flip local m $ do
        env <- ask
        let ctx = getMap $ defCtx env
        pure $ env { defCtx = TypeEnv $ getMap (remove (TypeEnv ctx) x `extend` (x, sc)) `Map.union` ctx }

inEnvMany :: [(String, Scheme)] -> Infer a -> Infer a
inEnvMany list m = do
    let map' = Map.fromList list
    local (\e -> e { defCtx = TypeEnv $ map' `Map.union` getMap (defCtx e) }) m

withLin :: (String, Linearity) -> Infer a -> Infer a
withLin nl i = do
    s <- gets linearities
    uncurry putLin nl
    res <- i
    modify $ \st -> st { linearities = s }
    pure res

withLinMany :: [(String, Linearity)] -> Infer a -> Infer a
withLinMany nls i = do
    s <- gets linearities
    mapM_ (uncurry putLin) nls
    res <- i
    modify $ \st -> st { linearities = s }
    pure res

-- | Lookup type in the environment
lookupEnv :: String -> Infer Type
lookupEnv x = do
    env <- asks (getMap . defCtx)
    env' <-  asks (getMap . ctorCtx)
    case Map.lookup x env <|> Map.lookup x env' of
        Nothing   ->  throwError $ makeUnboundVarError x
        Just s    ->  instantiate s

lookupLin :: String -> Infer (Maybe Linearity)
lookupLin x = gets (Map.lookup x . linearities)

putLin :: String -> Linearity -> Infer ()
putLin x l = modify $ \st -> st { linearities = Map.insert x l (linearities st) }

fresh :: String -> Infer Type
fresh v = do
    s <- get
    put s { count = count s + 1 }

    let id' = v <> show (count s)
    pure . TVar $ TV id'

instantiate :: Scheme -> Infer Type
instantiate (Scheme as t) = do
    as' <- mapM (const $ fresh "@") as
    let s = Map.fromList $ zip as as'
    pure $ apply s (relax t)

generalize :: TypeEnv -> Type -> Scheme
generalize env t  = Scheme as t
    where as = Set.toList $ ftv t `Set.difference` ftv env

rigidify :: Type -> Type
rigidify t = let subst = Map.fromList . Set.toList $ Set.map (\v -> (v, TRigid v)) (ftv t)
             in apply subst t

relax :: Type -> Type
relax (TRigid n)    = TVar n
relax (TFun t1 t2)  = TFun (relax t1) (relax t2)
relax (TTuple ts)   = TTuple (map relax ts)
relax (TApp t1 t2)  = TApp (relax t1) (relax t2)
relax (TBang t1)  = TBang (relax t1)
relax t = t

infer :: Annotated Expr -> Infer (Type, [Constraint])
infer (e :- _) = case e of
    ELit (LInt _) -> pure (TInt, [])
    ELit (LDec _) -> pure (TFloat, [])
    ELit (LChr _) -> pure (TChar, [])
    EHole -> do
        tv <- fresh "_"
        tv' <- fresh "#"
        pure (tv, [(tv, tv')])
    EId x -> do
        t <- lookupEnv x
        lookupLin x >>= \case
            Nothing -> pure (t, []) -- non linear variable (function or operator, nobody cares about them)
            Just Unrestricted -> pure (t, [])
            Just Linear -> putLin x Used $> (t, [])
            Just Used -> throwError $ makeTooMuchUsagesError x
    ELam x e' -> do
        (pat, cs, env) <- inferPattern x

        let convertToLin name (Scheme _ t) = (name,) $ case t of
                TBang _ -> Unrestricted
                _ -> Linear
            lins = uncurry convertToLin <$> Map.toList env

        (t, c) <- inEnvMany (Map.toList env) (withLinMany lins $ infer e')

        pure (pat `TFun` t, cs <> c)
    EApp e1 e2 -> do
        (t1, c1) <- infer e1
        (t2, c2) <- infer e2
        tv <- fresh "#"
        pure (tv, c1 <> c2 <> [(t1, t2 `TFun` tv)])
    ETuple es -> do
        ts <- mapM infer es
        pure (TTuple $ map fst ts, foldMap snd ts)
    EAnn e t -> do
        (t', c) <- infer e
        pure (t', (tiType t, t') : c)
    EMatch e cases -> do
        (tExp, tCon) <- infer e

        res <- (unzip3 <$>) . forM cases $ \(pat, expr) -> do
            (patTy, patsCons, env) <- inferPattern pat

            let convertToLin name (Scheme _ t) = (name,) $ case t of
                    TBang _ -> Unrestricted
                    _ -> Linear
                lins = uncurry convertToLin <$> Map.toList env

            (exprTy, exprCons) <- inEnvMany (Map.toList env) (withLinMany lins $ infer expr)
            pure (exprTy, patTy, exprCons <> patsCons)

        let (ret:xs, patsTy, pCons) = res
            types = zipFrom ret xs <> zipFrom tExp patsTy
            cons = mconcat pCons

        pure (ret, types <> cons <> tCon)
      where
        zipFrom :: a -> [b] -> [(a, b)]
        zipFrom = zip . repeat

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
    PLinear (PId id' :- _) -> do
        t <- fresh "&"
        pure (TBang t, [], Map.singleton id' (Scheme [] (TBang t)))
    PAnn p t -> do
        (t', cs, env) <- inferPattern p
        let t'' = tiType t
        pure (t'', (t'', t') : cs, env)
    PLinear p -> do
        t <- fresh "&"
        inferPattern (PAnn p (untiType (TBang t) :- Nothing) :- Nothing)
    PCtor id' args -> do
        ctor <- instantiate =<< lookupCtor id'
        let (ts, r) = unfoldParams ctor

        guard (length args == length ts)
            <|> throwError (text "Expected " <> text (show $ length ts) <> text " arguments to constructor \"" <> text id' <> text "\", but got " <> text (show $ length args) <> dot <> linebreak)

        (ts', cons, env) <- invmap mconcat (: []) <$> mapAndUnzip3M inferPattern args

        let cons' = zip ts ts'

        pure (r, cons' <> mconcat cons, env)
  where
    lookupCtor :: String -> Infer Scheme
    lookupCtor id' = do
        env <- asks (getMap . ctorCtx)
        case Map.lookup id' env of
            Nothing -> throwError $ makeUnboundVarError id'
            Just x  -> pure x

    unfoldParams :: Type -> ([Type], Type)
    unfoldParams (TFun a b) = first (a:) (unfoldParams b)
    unfoldParams t = ([], t)


inferTop :: GlobalEnv -> [(String, Annotated Expr)] -> Either TIError GlobalEnv
inferTop env [] = Right env
inferTop env ((name, ex):xs) = case inferExpr env ex of
    Left err -> Left err
    Right ty -> inferTop (env { defCtx = extend (defCtx env) (name, ty) }) xs

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

normalize :: Scheme -> Scheme
normalize (Scheme _ body) = Scheme (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (map TV letters)

    fv (TVar a)    = [a]
    fv (TFun a b)  = fv a <> fv b
    fv (TApp a b)  = fv a <> fv b
    fv (TTuple e)  = foldMap fv e
    fv (TBang t) = fv t
    fv _           = []

    normtype (TFun a b)       = TFun (normtype a) (normtype b)
    normtype (TApp a b)       = TApp (normtype a) (normtype b)
    normtype (TTuple e)       = TTuple (map normtype e)
    normtype (TBang t)      = TBang (normtype t)
    normtype (TVar a@(TV x')) =
        case Prelude.lookup a ord of
            Just x -> TRigid x
            Nothing -> error $ "The type variable \"" <> x' <> "\" has not been declared in type, but wants to be used.\n"
                                <> "This error should never happen. If you see it, please report it."
    normtype t          = t

-- | Run the constraint solver
runSolve :: GlobalEnv -> [Constraint] -> Either TIError Subst
runSolve g cs = runReader (runExceptT $ solver st) g
  where st = (nullSubst, cs)

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany []         []         = pure nullSubst
unifyMany (t1 : ts1) (t2 : ts2) = do
    su1 <- unifies t1 t2
    su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
    pure (su2 `compose` su1)
unifyMany t1 t2 = throwError $ foldl (\acc (t1', t2') -> acc <> makeUnifyError t1' t2') empty (zip t1 t2)

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2          = pure nullSubst
unifies (TVar v) t                = v `bind` t
unifies t            (TVar v    ) = v `bind` t
unifies (TBang t1) (TBang t2) = unifies t1 t2
unifies (TId i) t       = unifyAlias i t
unifies t       (TId i) = unifyAlias i t
unifies (TFun t1 t2) (TFun t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies (TTuple e) (TTuple e')    = unifyMany e e'
unifies (TApp t1 t2) (TApp t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies a@(TApp _ _) t = unifyCustom a t
unifies t a@(TApp _ _) = unifyCustom a t
unifies t1           t2           =
    let (Scheme _ st1) = closeOver t1
        (Scheme _ st2) = closeOver t2
    in throwError $ makeUnifyError st1 st2

unifyCustom :: Type -> Type -> Solve Subst
unifyCustom a@(TApp t1 t2) t3 = go t1 [t2]
  where go (TApp t1' t2') args = go t1' (t2':args)
        go (TId i) args = asks (Map.lookup i . typeDefCtx) >>= \case
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

unifyAlias :: String -> Type -> Solve Subst
unifyAlias name t1 =
    asks (Map.lookup name . typeDefCtx) >>= \case
        Just (CustomScheme _ (TAlias t2)) -> unifies t2 t1
        Just _ ->
            let (Scheme _ st1) = closeOver t1
            in throwError $ makeUnifyError (TId name) st1
        Nothing ->
            let (Scheme _ st1) = closeOver t1
            in throwError $ makeUnifyError (TId name) st1 -- ? Should never happen

-- Unification solver
solver :: Unifier -> Solve Subst
solver (su, cs) = case cs of
    []               -> pure su
    ((t1, t2) : cs0) -> do
        su1 <- unifies t1 t2
        solver (su1 `compose` su, apply su1 cs0)

bind :: TVar -> Type -> Solve Subst
bind a t | t == TVar a     = case a of TV (h:id') | h == '_' ->
                                                        let (Scheme _ st) = closeOver t
                                                        in throwError $ makeHoleError st
                                                  | otherwise -> pure nullSubst
         | occursCheck a t = throwError $ makeOccurError a t
         | otherwise       = pure $ Map.singleton a t

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t


-- type hole solver
runHoleInspect :: Subst -> Either TIError ()
runHoleInspect subst =
    let map' = Map.filterWithKey (\(TV k) _ -> head k == '_') subst
    in if null map'
        then pure ()
        else throwError $ Map.foldl (\acc t -> acc <> let (Scheme _ st) = closeOver t in makeHoleError st) empty map'

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

tiType :: Annotated TP.Type -> Type
tiType (TP.TId id' :- _)        = TId id'
tiType (TP.TFun t1 t2 :- _)     = TFun (tiType t1) (tiType t2)
tiType (TP.TTuple ts :- _)      = TTuple (map tiType ts)
tiType (TP.TRVar id' :- _)      = TRigid (TV id')
tiType (TP.TApp t1 t2 :- _)     = TApp (tiType t1) (tiType t2)
tiType (TP.TBang t :- _)      = TBang (tiType t)
tiType (TP.TVar id' :- _)       = TVar (TV id')

untiType :: Type -> TP.Type
untiType (TId i) = TP.TId i
untiType (TFun t1 t2) = TP.TFun (untiType t1 :- Nothing) (untiType t2 :- Nothing)
untiType (TTuple ts) = TP.TTuple ((:- Nothing) . untiType <$> ts)
untiType (TRigid (TV i)) = TP.TRVar i
untiType (TApp t1 t2) = TP.TApp (untiType t1 :- Nothing) (untiType t2 :- Nothing)
untiType (TBang t) = TP.TBang (untiType t :- Nothing)
untiType (TVar (TV i)) = TP.TVar i
untiType TInt = TP.TId "Integer"
untiType TChar = TP.TId "Char"
untiType TFloat = TP.TId "Double"

tiScheme :: TP.Scheme -> Scheme
tiScheme (TP.Scheme tvs t) = Scheme (map TV tvs) (tiType t)

tiCustomType :: Annotated TP.CustomType -> CustomType
tiCustomType (TP.TSum cs :- _)   = TSum (fmap tiScheme cs)
tiCustomType (TP.TAlias t :- _)  = TAlias (tiType t)

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

handleStatement :: String -> These (Annotated Expr) (Annotated TP.Type) -> Check ()
handleStatement name (This def)      = do
    e <- get
    inferDef e name def
handleStatement name (That _)        = throwError $ makeBindLackError name
handleStatement name (These def typ) = do
    let ti     = tiType typ
    let gen'ed = closeOver (relax ti)
    checkKI $ kiScheme gen'ed

    e <- get
    inferDef e name def

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

    modify $ \st -> st { typeDefCtx  = Map.insert k v (typeDefCtx st)
                       , typeDeclCtx = Map.insert k kind (typeDeclCtx st)
                       , ctorCtx     = let (TypeEnv env) = ctorCtx st
                                       in TypeEnv (schemes `Map.union` env) }

unfoldParams :: Type -> ([Type], Type)
unfoldParams (TFun a b) = first (a:) (unfoldParams b)
unfoldParams t = ([], t)

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

programTypeInference :: GlobalEnv -> Check a -> Either TIError (a, GlobalEnv)
programTypeInference g p = runExcept (runStateT p g)