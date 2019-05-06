module Blob.Inference.AlgorithmW 
( Type(..)
, TIError
, TypeEnv(..)
, GlobalEnv(..)
, runTI
, checkTI
, typeInference
, tiProgram
, programTypeInference
)
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.MultiMap as MMap
import Blob.Inference.Types (Type(..), Subst, Scheme(..), TypeEnv(..), TIError, TI, TIState(..), Types(..), Check, GlobalEnv(..))
import Blob.Parsing.Types (Expr(..), Literal(..), Statement(..), Program(..))
import Blob.PrettyPrinter.PrettyInference (pType)
import qualified Blob.Parsing.Types as TP (Type(..))
import Control.Monad (zipWithM, foldM)
import Control.Monad.State (runState, get, put, modify, runStateT)
import Control.Monad.Reader (runReaderT, ask, local)
import Control.Monad.Except (runExceptT, throwError, runExcept)
import Text.PrettyPrint.Leijen (text, dot, linebreak)
import Data.Bifunctor (bimap, first, second)
import Data.Maybe (isJust)

nullSubst :: Subst
nullSubst = mempty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

insert :: String -> Scheme -> TypeEnv -> TypeEnv
insert k v (TypeEnv env) = TypeEnv $ Map.insert k v env

lookup' :: TypeEnv -> String -> Maybe Scheme
lookup' (TypeEnv env) n = Map.lookup n env

getScheme :: String -> TypeEnv -> Maybe Scheme
getScheme k (TypeEnv env) = env Map.!? k

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
    where vars = Set.toList (ftv t Set.\\ ftv env)

runTI :: TypeEnv -> TI a -> (Either TIError a, TIState)
runTI env t = runState (runReaderT (runExceptT t) env) initTIState
  where initTIState = TIState { tiSupply = 0
                              , tiSubst = mempty }

newTyVar :: String -> TI Type
newTyVar prefix = do
    s <- get
    put s { tiSupply = tiSupply s + 1 }
    pure $ TVar (prefix ++ show (tiSupply s))

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
    nvars <- mapM (\_ -> newTyVar "a") vars
    let s = Map.fromList (zip vars nvars)
    pure $ apply s (relax t)

rigidify :: Type -> Type
rigidify t = let subst = Map.fromList . Set.toList $ Set.map (\v -> (v, TRigidVar v)) (ftv t)
             in apply subst t

relax :: Type -> Type
relax (TRigidVar n) = TVar n
relax (TFun t1 t2)  = TFun (relax t1) (relax t2)
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
mgu (TRigidVar u) TInt | u == "Integer"    = pure nullSubst
mgu (TRigidVar u) TString | u == "String"  = pure nullSubst
mgu (TRigidVar u) TFloat | u == "Float"    = pure nullSubst
mgu TInt (TRigidVar u) | u == "Integer"    = pure nullSubst
mgu TString (TRigidVar u) | u == "String"  = pure nullSubst
mgu TFloat (TRigidVar u) | u == "Float"    = pure nullSubst
mgu (TTuple ts1) (TTuple ts2)              = foldr composeSubst nullSubst <$> zipWithM mgu ts1 ts2
mgu (TList t1) (TList t2)                  = mgu t1 t2
mgu t1 t2                                  = throwError $ makeUnifyError t1 t2

mguScheme :: Scheme -> Scheme -> TI ()
mguScheme (Scheme tvs t) (Scheme tvs' t') = () <$ mgu t (apply subst t')
  where subst = Map.fromList [ (tv', TRigidVar tv) | (tv, tv') <- zip tvs tvs' ]

varBind :: String -> Type -> TI Subst
varBind u t | t == TVar u          = pure nullSubst
            | u `Set.member` ftv t = throwError $ makeOccurError u t
            | otherwise            = pure $ Map.singleton u t

tiExpr :: Expr -> TI (Subst, Type)
tiExpr (EId n)      = do
    (TypeEnv env) <- ask
    case Map.lookup n env of
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
    (s1, t1) <- local (insert n (Scheme [] tv)) (tiExpr e)
    pure (s1, TFun (apply s1 tv) t1)
tiExpr (EApp e1 e2) = do
    tv       <- newTyVar "a"
    (s1, t1) <- tiExpr e1
    (s2, t2) <- local (apply s1) (tiExpr e2)
    s3       <- mgu (apply s2 t1) (TFun t2 tv)
    pure (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
tiExpr (ETuple es)  = fmap TTuple <$> tiTuple es
  where
    tiTuple [] = pure (nullSubst, [])
    tiTuple (e:es') = do
        (s1, t)  <- tiExpr e
        (s2, ts) <- tiTuple es'
        pure (s2 `composeSubst` s1, t:ts)
tiExpr (EList es)   = flip (foldM go) es =<< (,) nullSubst <$> newTyVar "a"
  where
    go (subst, type') item = do
        (s1, t) <- tiExpr item
        s2      <- mgu t type'
        pure (s2 <> s1 <> subst, apply s2 type')


typeInference :: TypeEnv -> Expr -> TI Type
typeInference _ e = do
    (s, t) <- tiExpr e
    pure $ apply s t



makeUnifyError :: Type -> Type -> TIError
makeUnifyError t1 t2 = text "Could not match type “" <> pType t1 <> text "” with “" <> pType t2 <> text "”" <> dot <> linebreak
makeOccurError :: String -> Type -> TIError
makeOccurError s t1 = text "Occur check fails: " <> text s <> text " vs " <> pType t1 <> dot <> linebreak
makeUnboundVarError :: String -> TIError
makeUnboundVarError s = text "Unbound symbol “" <> text s <> text "”" <> dot <> linebreak
makeRedeclaredError :: String -> TIError
makeRedeclaredError id' = text "Symbol “" <> text id' <> text "” already declared" <> dot <> linebreak
makeRedefinedError :: String -> TIError
makeRedefinedError id' = text "Symbol “" <> text id' <> text "” already defined" <> dot <> linebreak




tiType :: TP.Type -> Type
tiType (TP.TId id')        = TRigidVar id'
tiType (TP.TArrow _ t1 t2) = TFun (tiType t1) (tiType t2)
tiType (TP.TTuple ts)      = TTuple (map tiType ts)
tiType (TP.TVar id')       = TRigidVar id'
tiType (TP.TList t)        = TList (tiType t)

sepStatements :: [Statement] -> Check ()
sepStatements s = do
    env <- get
    let (TypeEnv external) = declCtx env
        (types, exprs)     = bimap MMap.fromList MMap.fromList $ separateDecls s
    
        res                = Map.mapWithKey (\k v -> if length v > 1 || isJust (Map.lookup k external)
                                                     then throwError $ makeRedeclaredError k
                                                     else Right $ head v) (MMap.toMap types)
        res1               = Map.mapWithKey (\k v -> if length v > 1 || isJust (Map.lookup k external)
                                                     then throwError $ makeRedefinedError k
                                                     else Right $ head v) (MMap.toMap exprs)

    env <- get

    flip Map.traverseWithKey res $ \k v -> case v of
        Left err    -> throwError err
        Right type' -> case lookup' (declCtx env) k of
            Nothing -> analyseDecl k type'
            Just _  -> throwError $ makeRedeclaredError k

    flip Map.traverseWithKey res1 $ \k v -> case v of
        Left err    -> throwError err
        Right expr  -> case lookup' (defCtx env) k of
            Nothing -> analyseDef k expr
            Just _  -> throwError $ makeRedefinedError k

    pure ()
  where
    analyseDecl :: String -> TP.Type -> Check ()
    analyseDecl k v = do
        env <- get

        let type' = tiType v
            tEnv  = defCtx env
        case getScheme k tEnv of
            Nothing -> modify $ \st -> st { declCtx = insert k (generalize (TypeEnv mempty) type') (declCtx st) }
            Just s  -> do
                checkTI $ mguScheme (generalize (TypeEnv mempty) type') s
                modify $ \st -> st { declCtx = insert k (generalize (TypeEnv mempty) type') (defCtx st) }

    analyseDef :: String -> Expr -> Check ()
    analyseDef k v = do
        type' <- checkTI $ do
            var        <- newTyVar "a"
            (subst, t) <- local (insert k (Scheme [] var)) (tiExpr v)
            subst1     <- mgu (apply subst var) (apply subst t)

            pure $ apply (subst1 `composeSubst` subst) var

        env <- get
        let tEnv = defCtx env
        case getScheme k tEnv of
            Nothing            -> modify $ \st -> st { defCtx = insert k (generalize (TypeEnv mempty) type') (defCtx st) }
            Just (Scheme _ t)  -> do
                checkTI $ mgu type' (rigidify t)
                modify $ \st -> st { defCtx = insert k (generalize (TypeEnv mempty) type') (defCtx st) }

    -- separateDecls :: [Statement] -> ([(String, TP.Type)], [(String, Expr)])
    separateDecls []                              = mempty
    separateDecls (Declaration id' type' : stmts) = first ((id', type'):) (separateDecls stmts)
    separateDecls (Definition id' expr : stmts)   = second ((id', expr):) (separateDecls stmts)
    separateDecls (_ : stmts)                     = separateDecls stmts


checkTI :: TI a -> Check a
checkTI ti = do
    (GlobalEnv declEnv defEnv) <- get

    let (TypeEnv e1_) = declEnv
        (TypeEnv e2_) = defEnv
        env           = Map.unionWith const e1_ e2_
        res           = runTI (TypeEnv env) ti

    case fst res of
        Left err     -> throwError err
        Right result -> pure result

tiProgram :: Program -> Check ()
tiProgram (Program stmts) = sepStatements stmts

programTypeInference :: GlobalEnv -> Check a -> Either TIError (a, GlobalEnv)
programTypeInference g p = runExcept (runStateT p g)