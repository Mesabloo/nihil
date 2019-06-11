{-# LANGUAGE LambdaCase, TupleSections #-}

module Blob.Inference.AlgorithmW 
( runTI
, checkTI
, typeInference
, tiProgram
, programTypeInference
, tiType
, tiScheme
, tiCustomType
, generalize
) where

import qualified Data.Map.Unordered as Map
import qualified Data.Map as Map'
import qualified Data.Set as Set
import qualified Data.MultiMap as MMap
import Blob.Inference.Types
import Blob.KindChecking.Checker
import Blob.KindChecking.Types
import Blob.Parsing.Types (Expr(..), Literal(..), Statement(..), Program(..), Pattern(..))
import Blob.PrettyPrinter.PrettyInference (pType)
import qualified Blob.Parsing.Types as TP (Type(..), Scheme(..), CustomType(..))
import Control.Monad (zipWithM, foldM, forM, mapAndUnzipM, guard)
import Control.Monad.Trans (lift)
import Control.Monad.State (runState, get, put, modify, runStateT,gets)
import Control.Monad.Reader (runReaderT, ask, local, asks)
import Control.Monad.Except (runExceptT, throwError, runExcept)
import Text.PrettyPrint.Leijen (text, dot, linebreak)
import Data.Bifunctor (bimap, first, second)
import Data.Maybe (isJust)
import Data.Align.Key (alignWithKey)
import Data.These (These(..))
import Control.Applicative ((<|>), liftA2)
import Debug.Trace
import Data.List.Extra (snoc)
import MonadUtils (mapAndUnzip3M)
import Data.Tuple.Utils (fst3, snd3, thd3)
import Data.Composition ((.:))

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
    where vars = Set.toList (ftv t Set.\\ ftv env)

runTI :: GlobalEnv -> TI a -> (Either TIError a, TIState)
runTI env t = runState (runReaderT (runExceptT t) env) initTIState
  where initTIState = TIState { tiSupply = 0
                              , tiSubst = mempty }

newTyVar :: String -> TI Type
newTyVar prefix = do
    s <- get
    put s { tiSupply = tiSupply s + 1 }
    pure $ TVar (prefix <> show (tiSupply s))

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
    nvars <- mapM (\_ -> newTyVar "a") vars
    let s = Map'.fromList (zip vars nvars)
    pure $ apply s (relax t)

rigidify :: Type -> Type
rigidify t = let subst = Map'.fromList . Set.toList $ Set.map (\v -> (v, TRigidVar v)) (ftv t)
             in apply subst t

relax :: Type -> Type
relax (TRigidVar n) = TVar n
relax (TFun t1 t2)  = TFun (relax t1) (relax t2)
relax (TTuple ts)   = TTuple (map relax ts)
relax (TApp t1 t2)  = TApp (relax t1) (relax t2)
relax t             = t

mgu :: Type -> Type -> TI Subst
mgu (TFun l r) (TFun l' r')                = do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    pure $ s1 `composeSubst` s2
mgu (TVar u) t                             = varBind u t
mgu t (TVar u)                             = varBind u t
mgu TInt TInt                              = pure nullSubst
mgu TString TString                        = pure nullSubst
mgu TFloat TFloat                          = pure nullSubst
mgu (TRigidVar u) (TRigidVar u') | u == u' = pure nullSubst
mgu (TId u) (TId u') | u == u'             = pure nullSubst
mgu (TId u) TInt | u == "Integer"          = pure nullSubst
mgu (TId u) TString | u == "String"        = pure nullSubst
mgu (TId u) TFloat | u == "Float"          = pure nullSubst
mgu TInt (TId u) | u == "Integer"          = pure nullSubst
mgu TString (TId u) | u == "String"        = pure nullSubst
mgu TFloat (TId u) | u == "Float"          = pure nullSubst
mgu (TTuple ts1) (TTuple ts2)              = foldr composeSubst nullSubst <$> zipWithM mgu ts1 ts2
-- mgu TList TList                            = pure nullSubst
mgu (TApp t1 t2) (TApp t1' t2')            = composeSubst <$> mgu t1 t1' <*> mgu t2 t2'
mgu t1 t2                                  = throwError $ makeUnifyError t1 t2

mguScheme :: Scheme -> Scheme -> TI ()
mguScheme (Scheme tvs t) (Scheme tvs' t') = () <$ mgu t (apply subst t')
  where subst = Map'.fromList [ (tv', TRigidVar tv) | (tv, tv') <- zip tvs tvs' ]

varBind :: String -> Type -> TI Subst
varBind u t | t == TVar u          = pure nullSubst
            | u `Set.member` ftv t = throwError $ makeOccurError u t
            | otherwise            = pure $ Map'.singleton u t

tiExpr :: Expr -> TI (Subst, Type)
tiExpr (EId n)      = do
    (TypeEnv env)  <- asks defCtx
    (TypeEnv env1) <- asks ctorCtx
    case Map'.lookup n env <|> Map'.lookup n env1 of
        Nothing    -> throwError $ makeUnboundVarError n
        Just sigma -> do
            t <- instantiate sigma
            pure (nullSubst, t)
tiExpr (ELit l)     = do
    env <- ask
    tiLit env l
  where
    tiLit _ (LInt _) = return (nullSubst, TInt)
    tiLit _ (LStr _) = return (nullSubst, TString)
    tiLit _ (LDec _) = return (nullSubst, TFloat)
tiExpr (ELam n e)   = do
    tv       <- newTyVar "a"
    (s1, t1) <- local (insertFun n (Scheme [] tv)) (tiExpr e)
    pure (s1, TFun (apply s1 tv) t1)
tiExpr (EApp e1 e2) = do
    tv       <- newTyVar "a"
    (s1, t1) <- tiExpr e1
    (s2, t2) <- local (\ e -> e { defCtx = apply s1 (defCtx e) }) (tiExpr e2)
    s3       <- mgu (apply s2 t1) (TFun t2 tv)
    pure (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
tiExpr (ETuple es)  = fmap TTuple <$> tiTuple es
  where
    tiTuple [] = pure (nullSubst, [])
    tiTuple (e:es') = do
        (s1, t)  <- tiExpr e
        (s2, ts) <- tiTuple es'
        pure (s2 `composeSubst` s1, t:ts)
-- tiExpr (EList es)   = fmap (TApp TList) <$> (flip (foldM go) es =<< (,) nullSubst <$> newTyVar "a")
--   where
--     go (subst, type') item = do
--         (s1, t) <- tiExpr item
--         s2      <- mgu t type'
--         pure (s2 <> s1 <> subst, apply s2 type')
tiExpr (EMatch e cases) = do
    (sub, ty)    <- tiExpr e
    patterns     <- mapM (tiPattern . fst) cases -- :: [(Subst, Type, TypeEnv)]
    let branches         = map snd cases         -- :: [(Pattern, Expr)]

        ( patternSubst                           -- :: [Subst]
         , patternTypes                          -- :: [Type]
         , patternEnvs ) = unzip3 patterns       -- :: [TypeEnv]

        pSubs            = patternSubst
        branches'        = zip branches patternEnvs
        pSub'            = foldr composeSubst nullSubst pSubs
        toSchemes        = fmap (TypeEnv . Map'.mapWithKey (Scheme [] .: flip const)) <$> branches'

    (subs, ts) <- unzip <$> mapM (uncurry inferBranch) toSchemes

    (bSub, r)  <- unifyRSide ts

    let subs'            = foldr composeSubst nullSubst subs
        pbSub            = subs' `composeSubst` pSub' `composeSubst` bSub

    pSub       <- unifyLSide (apply pbSub ty) patternTypes

    let subst            = foldr composeSubst sub (pbSub:pSub:pSubs)

    pure (subst, apply subst r)
  where unifyLSide :: Type -> [Type] -> TI Subst
        unifyLSide exprTy = foldM (\acc t -> composeSubst acc <$> mgu t (apply acc exprTy)) nullSubst

        inferBranch :: Expr -> TypeEnv -> TI (Subst, Type)
        inferBranch e env = local (insertEnv env) (tiExpr e)

        unifyRSide :: [Type] -> TI (Subst, Type)
        unifyRSide (t:ts) = do
            subst <- foldM (\acc t' -> composeSubst acc <$> mgu t' (apply acc t)) nullSubst ts
            pure (subst, apply subst t)
        unifyRSide []     = (nullSubst,) <$> newTyVar "a"

tiPattern :: Pattern -> TI (Subst, Type, Map'.Map String Type)
tiPattern = \case
    Wildcard -> do
        var <- newTyVar "p"
        pure (nullSubst, var, mempty)
    PInt _         -> pure (nullSubst, TInt, mempty)
    PDec _         -> pure (nullSubst, TFloat, mempty)
    PStr _         -> pure (nullSubst, TString, mempty)
    PId id'        -> do
        var <- newTyVar "p"
        pure (nullSubst, var, Map'.singleton id' var)
    PCtor id' args -> do
        x             <- instantiate =<< lookupCtor id'
        let (ts, r) = unfoldParams x
        guard (length args == length ts)
            <|> throwError (text "Too much or not enough arguments to the type constructor" <> dot <> linebreak)
        (s, ts', env) <- third mconcat <$> mapAndUnzip3M tiPattern args

        let s'      = foldl composeSubst nullSubst s
        subst         <- foldM (\acc (t1, t2) -> composeSubst acc <$> mgu t2 (apply acc t1))
            nullSubst
            (zip ts ts')
        let s''     = s' `composeSubst` subst

        pure (s'', apply s'' r, env)
  where unfoldParams :: Type -> ([Type], Type)
        unfoldParams (TFun a b) = first (a:) (unfoldParams b)
        unfoldParams t          = ([], t)

        lookupCtor :: String -> TI Scheme
        lookupCtor id' = do
            (TypeEnv ctor) <- asks ctorCtx
            case Map'.lookup id' ctor of
                Nothing -> throwError $ makeUnboundVarError id'
                Just s  -> pure s

        third :: (c -> c') -> (a, b, c) -> (a, b, c')
        third f (a, b, c) = (a, b, f c)

typeInference :: TypeEnv -> Expr -> TI Type
typeInference _ e = do
    (s, t) <- tiExpr e
    pure $ apply s t



makeUnifyError :: Type -> Type -> TIError
makeUnifyError t1 t2 = text "Could not match type “" <> pType t1 <> text "” with “" <> pType t2 <> text "”" <> dot <> linebreak
makeOccurError :: String -> Type -> TIError
makeOccurError s t1 = text "Occur check fails: cannot construct the infinite type “" <> text s <> text " ~ " <> pType t1 <> text "”" <> dot <> linebreak
makeUnboundVarError :: String -> TIError
makeUnboundVarError s = text "Unbound symbol “" <> text s <> text "”" <> dot <> linebreak
makeRedeclaredError :: String -> TIError
makeRedeclaredError id' = text "Symbol “" <> text id' <> text "” already declared" <> dot <> linebreak
makeRedefinedError :: String -> TIError
makeRedefinedError id' = text "Symbol “" <> text id' <> text "” already defined" <> dot <> linebreak
makeBindLackError :: String -> TIError
makeBindLackError id' = text "“" <> text id' <> text "” lacks an accompanying definition" <> dot <> linebreak




tiType :: TP.Type -> Type
tiType (TP.TId id')        = TId id'
tiType (TP.TArrow _ t1 t2) = TFun (tiType t1) (tiType t2)
tiType (TP.TFun t1 t2)     = TFun (tiType t1) (tiType t2)
tiType (TP.TTuple ts)      = TTuple (map tiType ts)
tiType (TP.TVar id')       = TRigidVar id'
tiType (TP.TApp t1 t2)     = TApp (tiType t1) (tiType t2)

tiScheme :: TP.Scheme -> Scheme
tiScheme (TP.Scheme tvs t) = Scheme tvs (tiType t)

tiCustomType :: TP.CustomType -> CustomType
tiCustomType (TP.TSum cs)   = TSum (fmap tiScheme cs)
tiCustomType (TP.TProd c s) = TProd c (tiScheme s)
tiCustomType (TP.TAlias t)  = TAlias (tiType t)

sepStatements :: [Statement] -> Check (Map.Map String Expr, Map.Map String TP.Type) -- uncurry (liftA2 (,)) . 
sepStatements = uncurry (liftA2 (,)) . bimap (foldDecls makeRedeclaredError mempty) (foldDecls makeRedefinedError mempty) . separateDecls
  where
    separateDecls []                              = mempty
    separateDecls (Declaration id' type' : stmts) = second ((id', type'):) (separateDecls stmts)
    separateDecls (Definition id' expr : stmts)   = first ((id', expr):) (separateDecls stmts)
    separateDecls (_ : stmts)                     = separateDecls stmts

    foldDecls :: (String -> TIError) -> Map.Map String a -> [(String, a)] -> Check (Map.Map String a)
    foldDecls _ m []              = pure m
    foldDecls err m ((id', t):ts) = case Map.lookup id' m of
                                        Nothing -> flip (foldDecls err) ts $ Map.insert id' t m
                                        Just _ -> throwError $ err id'      

handleStatement :: String -> These Expr TP.Type -> Check ()
handleStatement name (This def)      = do
    t <- checkTI $ do
        var        <- newTyVar "a"
        (subst, t) <- local (insertFun name (Scheme [] var)) (tiExpr def)
        subst1     <- mgu (apply subst var) (apply subst t)
        pure $ apply (subst1 `composeSubst` subst) var

    env <- gets defCtx
    modify $ \st -> st { defCtx = insert name (generalize (TypeEnv mempty) t) env }
handleStatement name (That _)        = throwError $ makeBindLackError name
handleStatement name (These def typ) = do
    env <- gets typeDeclCtx

    let ti     = tiType typ
    let gen'ed = generalize (TypeEnv mempty) (relax ti)
    checkKI $ kiScheme gen'ed

    t <- checkTI $ do
        t <- do
            var        <- newTyVar "a"
            (subst, t) <- local (insertFun name (Scheme [] var)) (tiExpr def)
            subst1     <- mgu (apply subst var) (apply subst t)
            pure $ apply (subst1 `composeSubst` subst) var

        mgu t (rigidify ti)
        pure t
    
    env <- gets defCtx
    modify $ \st -> st { defCtx = insert name (generalize (TypeEnv mempty) t) env }

analyseTypeDecl :: String -> CustomScheme -> Check ()
analyseTypeDecl k v = do
    kind <- checkKI $ do
        var        <- newKindVar "r"
        (subst, t) <- local (Map'.insert k var) (kiCustomScheme v)
        subst1     <- mguKind (applyKind subst var) (applyKind subst t)
        pure $ applyKind (subst1 `composeKindSubst` subst) var

    let (CustomScheme _ t) = v
    schemes <- case t of
        TSum ctors -> pure ctors
        _          -> pure $ Map'.fromList []

    modify $ \st -> st { typeDefCtx  = Map'.insert k v (typeDefCtx st)
                       , typeDeclCtx = Map'.insert k kind (typeDeclCtx st)
                       , ctorCtx     = let (TypeEnv env) = ctorCtx st
                                       in TypeEnv (schemes `Map'.union` env) }

checkTI :: TI a -> Check a
checkTI ti = do
    gEnv <- get

    let res = runTI gEnv ti

    case fst res of
        Left err     -> throwError err
        Right result -> pure result

tiProgram :: Program -> Check ()
tiProgram (Program stmts) = do
    remaining <- sepTypeDecls stmts
    stts      <- sepStatements remaining
    sequence_ $ uncurry (alignWithKey handleStatement) stts
  where sepTypeDecls [] = pure []
        sepTypeDecls (TypeDeclaration k tvs t:ss) = do
            analyseTypeDecl k (CustomScheme tvs (tiCustomType t))
            sepTypeDecls ss
        sepTypeDecls (s:ss) = (s:) <$> sepTypeDecls ss

programTypeInference :: GlobalEnv -> Check a -> Either TIError (a, GlobalEnv)
programTypeInference g p = runExcept (runStateT p g)
