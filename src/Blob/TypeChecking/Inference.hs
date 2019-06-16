{-# LANGUAGE LambdaCase, TupleSections #-}

module Blob.TypeChecking.Inference where

import Blob.TypeChecking.Types
import Control.Monad.Except
import Data.Unique
import Control.Monad.State
import qualified Data.Set as Set
import Blob.TypeChecking.Errors
import qualified Data.Map as Map
import qualified Blob.Parsing.Types as PT
import Control.Monad.RWS
import Control.Monad.Identity

runInfer :: Infer a -> Either TIError (a, [Constraint])
runInfer m = runIdentity . runExceptT $ evalRWST m mempty initTiState
  where initTiState = InferState { count = 0 }

fresh :: String -> Infer Type
fresh v = do
    s <- get
    put s { count = count s + 1 }
    return . TVar . TV $ v <> show (count s)

bind :: TVar -> Type -> Infer Subst
bind a t | t == TVar a          = pure nullSubst
         | a `Set.member` ftv t = throwError $ makeOccurError a t
         | otherwise            = pure $ Map.singleton a t

instantiate :: Scheme -> Infer Type
instantiate (Scheme as t) = do
    as' <- mapM (const $ fresh "a") as
    let s = Map.fromList $ zip as as'
    return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme as t
  where as = Set.toList $ ftv t `Set.difference` ftv env


-- | Unify two types
uni :: Type -> Type -> Infer ()
uni t1 t2 = tell [(t1, t2)]

-- | Extend type environment
inEnv :: (String, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
    let scope e = remove e x `extend` (x, sc)
    local scope m

lookupEnv :: TypeEnv -> String -> Infer Type
lookupEnv (TypeEnv env) x = case Map.lookup x env of
    Nothing -> throwError $ makeUnboundVarError x
    Just s  -> instantiate s

infer :: PT.Expr -> Infer Type
infer = \case
    PT.ELit (PT.LInt _) -> pure $ TId "Integer"
    PT.ELit (PT.LDec _) -> pure $ TId "Double"
    PT.ELit (PT.LStr _) -> pure $ TId "String"
    PT.ELit (PT.LChr _) -> pure $ TId "Char"
    PT.EId x            -> do
        env <- ask
        lookupEnv env x
    PT.ELam x e         -> do
        tv <- fresh "a"
        t  <- inEnv (x, Scheme [] tv) (infer e)
        pure $ tv `TFun` t
    PT.EApp e1 e2 -> do
        t1 <- infer e1
        t2 <- infer e2
        tv <- fresh "a"
        uni t1 (t2 `TFun` tv)
        pure tv




unifies :: Type -> Type -> Solve Unifier
unifies t1 t2 | t1 == t2          = pure emptyUnifier
unifies (TVar v)     t            = do
    let t' = runInfer $ v `bind` t
    case t' of
        Left err -> throwError err
        Right x  -> pure x
unifies t            (TVar v    ) = do
    let t' = runInfer $ v `bind` t
    case t' of
        Left err -> throwError err
        Right x  -> pure x

unifies (TFun t1 t2) (TFun t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1           t2           = throwError $ makeUnifyError t1 t2

unifyMany :: [Type] -> [Type] -> Solve Unifier
unifyMany []         []         = pure emptyUnifier
unifyMany (t1 : ts1) (t2 : ts2) = do
  (su1, cs1) <- unifies t1 t2
  (su2, cs2) <- unifyMany (apply su1 ts1) (apply su1 ts2)
  pure (su2 `compose` su1, cs1 ++ cs2)
unifyMany t1 t2 = throwError $ makeUnifyError (head t1) (head t2)

-- Unification solver
solver :: Solve Subst
solver = do
    (su, cs) <- get
    case cs of
        []               -> return su
        ((t1, t2) : cs0) -> do
            (su1, cs1) <- unifies t1 t2
            put (su1 `compose` su, cs1 <> apply su1 cs0)
            solver
