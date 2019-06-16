{-# LANGUAGE LambdaCase, TupleSections #-}

module Blob.TypeChecking.Inference where

import Blob.TypeChecking.Types
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Set as Set
import Blob.TypeChecking.Errors
import qualified Data.Map as Map
import  Blob.Parsing.Types hiding (Type(..), Scheme)
import Control.Monad.RWS
import Control.Monad.Identity
import Data.List (nub)

-- | Run the inference monad
runInfer :: TypeEnv -> Infer (Type, [Constraint]) -> Either TIError ((Type, [Constraint]), [Constraint])
runInfer env m = runIdentity . runExceptT $ evalRWST m env initInfer
  where initInfer = InferState { count = 0 }

-- | Solve for the toplevel type of an expression in a given environment
inferExpr :: TypeEnv -> Expr -> Either TIError Scheme
inferExpr env ex = case runInfer env (infer ex) of
    Left err -> Left err
    Right ((ty, c), _) -> case runSolve c of
        Left err -> Left err
        Right subst -> Right . closeOver $ apply subst ty

-- | Return the internal constraints used in solving for the type of an expression
constraintsExpr :: TypeEnv -> Expr -> Either TIError ([Constraint], Subst, Type, Scheme)
constraintsExpr env ex = case runInfer env (infer ex) of
    Left err -> Left err
    Right ((ty, c), _) -> case runSolve c of
        Left err -> Left err
        Right subst -> Right (c, subst, ty, sc)
          where sc = closeOver $ apply subst ty

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> Scheme
closeOver = normalize . generalize mempty

-- | Extend type environment
inEnv :: (String, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
    let scope e = remove e x `extend` (x, sc)
    local scope m

-- | Lookup type in the environment
lookupEnv :: String -> Infer Type
lookupEnv x = do
    (TypeEnv env) <- ask
    case Map.lookup x env of
        Nothing   ->  throwError $ makeUnboundVarError x
        Just s    ->  instantiate s

fresh :: String -> Infer Type
fresh v = do
    s <- get
    put s { count = count s + 1 }
    pure . TVar . TV $ v <> show (count s)

instantiate :: Scheme -> Infer Type
instantiate (Scheme as t) = do
    as' <- mapM (const $ fresh "a") as
    let s = Map.fromList $ zip as as'
    pure $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t  = Scheme as t
    where as = Set.toList $ ftv t `Set.difference` ftv env

infer :: Expr -> Infer (Type, [Constraint])
infer = \case
    ELit (LInt _) -> pure (TId "Integer", [])
    ELit (LDec _) -> pure (TId "Double", [])
    ELit (LStr _) -> pure (TId "String", [])
    ELit (LChr _) -> pure (TId "Char", [])
    EId x -> do
        t <- lookupEnv x
        pure (t, [])
    ELam x e -> do
        tv <- fresh "a"
        (t, c) <- inEnv (x, Scheme [] tv) (infer e)
        pure (tv `TFun` t, c)
    EApp e1 e2 -> do
        (t1, c1) <- infer e1
        (t2, c2) <- infer e2
        tv <- fresh "a"
        pure (tv, c1 <> c2 <> [(t1, t2 `TFun` tv)])
    ETuple es -> do
        ts <- mapM infer es
        pure (TTuple $ map fst ts, foldl (\acc c -> acc <> snd c) [] ts)

inferTop :: TypeEnv -> [(String, Expr)] -> Either TIError TypeEnv
inferTop env [] = Right env
inferTop env ((name, ex):xs) = case inferExpr env ex of
    Left err -> Left err
    Right ty -> inferTop (extend env (name, ty)) xs

letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']

normalize :: Scheme -> Scheme
normalize (Scheme _ body) = Scheme (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (map TV letters)

    fv (TVar a)   = [a]
    fv (TFun a b) = fv a <> fv b
    fv (TId _)    = []
    fv _          = []

    normtype (TFun a b) = TFun (normtype a) (normtype b)
    normtype (TVar a)   =
        case Prelude.lookup a ord of
            Just x -> TVar x
            Nothing -> error "type variable not in signature"
    normtype t          = t

-- | Run the constraint solver
runSolve :: [Constraint] -> Either TIError Subst
runSolve cs = runIdentity . runExceptT $ solver st
  where st = (nullSubst, cs)

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany []         []         = pure nullSubst
unifyMany (t1 : ts1) (t2 : ts2) = do
    su1 <- unifies t1 t2
    su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
    pure (su2 `compose` su1)
unifyMany t1 t2 = throwError $ makeUnifyError (head t1) (head t2)

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2          = pure nullSubst
unifies (TVar v)     t            = v `bind` t
unifies t            (TVar v    ) = v `bind` t
unifies (TFun t1 t2) (TFun t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1           t2           = throwError $ makeUnifyError t1 t2

-- Unification solver
solver :: Unifier -> Solve Subst
solver (su, cs) = case cs of
    []               -> pure su
    ((t1, t2) : cs0) -> do
        su1 <- unifies t1 t2
        solver (su1 `compose` su, apply su1 cs0)

bind :: TVar -> Type -> Solve Subst
bind a t | t == TVar a     = pure nullSubst
         | occursCheck a t = throwError $ makeOccurError a t
         | otherwise       = pure $ Map.singleton a t

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t