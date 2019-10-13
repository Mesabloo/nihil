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
import Data.Functor (($>), (<&>))
import Data.Maybe
import Control.Lens

-- | Runs the inference monad given as argument.
runInfer :: GlobalEnv -> Infer (Type, [Constraint]) -> Either TIError ((Type, [Constraint]), [Constraint])
runInfer env m = runIdentity . runExceptT $ evalRWST m env initInfer
  where initInfer = InferState { _count = 0, _linearities = mempty }

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
                    defCtx %= (TypeEnv . (Map.singleton name (closeOver (apply x t)) `Map.union`) . getMap)


-- | Returns the internal constraints used in solving for the type of an expression.
constraintsExpr :: GlobalEnv -> Annotated Expr -> Either TIError ([Constraint], Subst, Type, Scheme)
constraintsExpr env ex = case runInfer env (infer ex) of
    Left err -> Left err
    Right ((ty, c), _) -> case runSolve env c of
        Left err -> Left err
        Right subst -> Right (c, subst, ty, sc)
          where sc = closeOver $ apply subst ty

-- | Canonicalizes and returns the polymorphic toplevel type.
closeOver :: Type -> Scheme
closeOver = normalize . generalize mempty

-- | Extends the type environment with a single entry.
inEnv :: (String, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m =
    flip local m $ do
        env <- ask
        let ctx = env ^. defCtx . to getMap
        pure $ env & defCtx .~ (TypeEnv $ getMap (remove (TypeEnv ctx) x `extend` (x, sc)) `Map.union` ctx)

-- | Extends the type environment with multiple entries.
inEnvMany :: [(String, Scheme)] -> Infer a -> Infer a
inEnvMany list m = do
    let map' = Map.fromList list
    local (defCtx %~ TypeEnv . (map' `Map.union`) . getMap) m

-- | Extends the linearity state with a single entry.
withLin :: (String, Linearity) -> Infer a -> Infer a
withLin nl i = do
    s <- use linearities
    uncurry putLin nl
    i <* (linearities .= s)

-- | Extends the linearity state with multiple entries.
withLinMany :: [(String, Linearity)] -> Infer a -> Infer a
withLinMany nls i = do
    s <- use linearities
    mapM_ (uncurry putLin) nls
    i <* (linearities .= s)

-- | Returns the type of a constant/function from the environment.
lookupEnv :: String -> Infer Type
lookupEnv x = do
    env <- defCtx `views` (Map.lookup x . getMap)
    env' <- ctorCtx `views` (Map.lookup x . getMap)
    case env <|> env' of
        Nothing   ->  throwError $ makeUnboundVarError x
        Just s    ->  instantiate s

-- | Returns the 'Linearity' of a constant from the state.
lookupLin :: String -> Infer (Maybe Linearity)
lookupLin x = linearities `uses` Map.lookup x

-- | Puts a 'Linearity' into the state.
putLin :: String -> Linearity -> Infer ()
putLin x l = linearities %= Map.insert x l

-- | Gets the 'Linearity' from a 'Scheme'.
getLinearity :: String -> Scheme -> (String, Linearity)
getLinearity name (Scheme _ t) = (name,) $ case t of
        TBang _ -> Unrestricted
        _ -> Linear

-- | Creates a new type variable with a given prefix.
fresh :: String -> Infer Type
fresh v = do
    s <- use count
    count += 1

    let id' = v <> show s
    pure . TVar $ TV id'

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
relax (TFun t1 t2)  = TFun (relax t1) (relax t2)
relax (TTuple ts)   = TTuple (map relax ts)
relax (TApp t1 t2)  = TApp (relax t1) (relax t2)
relax (TBang t1)  = TBang (relax t1)
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
    EId x -> do
        t <- lookupEnv x
        lookupLin x >>= \case
            Nothing -> pure (t, []) -- non linear variable (function or operator, nobody cares about them)
            Just Unrestricted -> pure (t, [])
            Just Linear -> putLin x Used $> (t, [])
            Just Used -> throwError $ makeTooMuchUsagesError x
    ELam x e' -> do
        (pat, cs, env) <- inferPattern x

        let lins = uncurry getLinearity <$> Map.toList env

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
            lins = uncurry getLinearity <$> Map.toList env

        (t3, c3) <- inEnvMany (Map.toList env) (withLinMany lins $ infer e)
        pure (t3, cs <> c3)
    EMatch e cases -> do
        (tExp, tCon) <- infer e

        res <- (unzip3 <$>) . forM cases $ \(pat, expr) -> do
            (patTy, patsCons, env) <- inferPattern pat

            let lins = uncurry getLinearity <$> Map.toList env

            (exprTy, exprCons) <- inEnvMany (Map.toList env) (withLinMany lins $ infer expr)
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
    -- | Returns the 'Scheme' of a constructor.
    lookupCtor :: String -> Infer Scheme
    lookupCtor id' = do
        env <- ctorCtx `views` (Map.lookup id' . getMap)
        case env of
            Nothing -> throwError $ makeUnboundVarError id'
            Just x  -> pure x

-- | An infinite stream of letters, looping on the alphabet.
letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

-- | Gives fresh new rigid type variables to a 'Scheme'.
normalize :: Scheme -> Scheme
normalize (Scheme _ body) = Scheme (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (map TV letters)

    fv (TVar a)    = [a]
    fv (TFun a b)  = fv a <> fv b
    fv (TApp a b)  = fv a <> fv b
    fv (TTuple e)  = foldMap fv e
    fv (TBang t)   = fv t
    fv _           = []

    normtype (TFun a b)       = TFun (normtype a) (normtype b)
    normtype (TApp a b)       = TApp (normtype a) (normtype b)
    normtype (TTuple e)       = TTuple (map normtype e)
    normtype (TBang t)        = TBang (normtype t)
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
tiType (TP.TFun t1 t2 :- _)     = TFun (tiType t1) (tiType t2)
tiType (TP.TTuple ts :- _)      = TTuple (map tiType ts)
tiType (TP.TRVar id' :- _)      = TRigid (TV id')
tiType (TP.TApp t1 t2 :- _)     = TApp (tiType t1) (tiType t2)
tiType (TP.TBang t :- _)        = TBang (tiType t)
tiType (TP.TVar id' :- _)       = TVar (TV id')

-- | Transforms a 'Type' into a 'TP.Type', only used when type checking the pattern 'PLinear'.
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
    ctorCtx %= (TypeEnv . (schemes `Map.union`) . getMap)

-- | Unfold the parameters and the return type from a type.
unfoldParams :: Type -> ([Type], Type)
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