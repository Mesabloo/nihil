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
import Text.PrettyPrint.Leijen (text, dot, linebreak, empty)
import Control.Monad.Reader
import MonadUtils (mapAndUnzip3M)
import Control.Applicative ((<|>))
import Data.Bifunctor (first, second)
import Debug.Trace

-- | Run the inference monad
runInfer :: GlobalEnv -> Infer (Type, [Constraint]) -> Either TIError ((Type, [Constraint]), [Constraint])
runInfer env m = runIdentity . runExceptT $ evalRWST m env initInfer
  where initInfer = InferState { count = 0 }

-- | Solve for the toplevel type of an expression in a given environment
inferExpr :: GlobalEnv -> Expr -> Either TIError Scheme
inferExpr env ex = case runInfer env (infer ex) of
    Left err -> Left err
    Right ((ty, c), _) -> case runSolve c of
        Left err -> Left err
        Right subst -> Right . closeOver $ apply subst ty

-- | Return the internal constraints used in solving for the type of an expression
constraintsExpr :: GlobalEnv -> Expr -> Either TIError ([Constraint], Subst, Type, Scheme)
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
inEnv (x, sc) m = 
    flip local m $ do
        env <- ask
        let ctx = getMap $ defCtx env
        pure $ env { defCtx = TypeEnv $ ctx `Map.union` getMap (remove (TypeEnv ctx) x `extend` (x, sc)) }

inEnvMany :: [(String, Scheme)] -> Infer a -> Infer a
inEnvMany list m = do
    let map' = Map.fromList list
    local (\e -> e { defCtx = TypeEnv $ map' `Map.union` getMap (defCtx e) }) m

-- | Lookup type in the environment
lookupEnv :: String -> Infer Type
lookupEnv x = do
    env <- asks (getMap . defCtx)
    env' <-  asks (getMap . ctorCtx)
    case Map.lookup x env <|> Map.lookup x env' of
        Nothing   ->  throwError $ makeUnboundVarError x
        Just s    ->  instantiate s

fresh :: String -> Infer Type
fresh v = do
    s <- get
    put s { count = count s + 1 }
    pure . TVar . TV $ v <> show (count s)

instantiate :: Scheme -> Infer Type
instantiate (Scheme as t) = do
    as' <- mapM (const $ fresh "i") as
    let s = Map.fromList $ zip as as'
    pure $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t  = Scheme as t
    where as = Set.toList $ ftv t `Set.difference` ftv env

infer :: Expr -> Infer (Type, [Constraint])
infer = \case
    ELit (LInt _) -> pure (TInt, [])
    ELit (LDec _) -> pure (TFloat, [])
    ELit (LStr _) -> pure (TString, [])
    ELit (LChr _) -> pure (TChar, [])
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
    EMatch e cases -> do
        (tExp, tCon) <- infer e
        let (pats, branches) = unzip cases
        (patsTy, patsCons, envs) <- unzip3 <$> mapM inferPattern pats
        let envBranch = zip envs branches
            types = zipFrom tExp patsTy

        xs <- forM envBranch $ \(env, expr) -> inEnvMany (Map.toList env) (infer expr)
        let ret = fst $ head xs

        let types2 = zipFrom ret (map fst $ tail xs)
            cons = mconcat (map snd xs)

        pure (ret, types2 <> cons <> types <> mconcat patsCons <> tCon)
      where inferPattern :: Pattern -> Infer (Type, [Constraint], Map.Map String Scheme)
            inferPattern = \case
                Wildcard -> do
                    t <- fresh "p"
                    pure (t, [], mempty)
                PInt _ -> pure (TInt, [], mempty)
                PStr _ -> pure (TString, [], mempty)
                PDec _ -> pure (TFloat, [], mempty)
                PChr _ -> pure (TChar, [], mempty)
                PId id' -> do
                    t <- fresh "p"
                    pure (t, [], Map.singleton id' (Scheme [] t))
                PTuple exp -> do
                    pats <- mapM inferPattern exp
                    let (ts, cs, envs) = unzip3 pats
                    pure (TTuple ts, mconcat cs, mconcat envs)
                PCtor id' args -> do
                    ctor <- instantiate =<< lookupCtor id'
                    let (ts, r) = unfoldParams ctor

                    guard (length args == length ts)
                        <|> throwError (text "Expected " <> text (show $ length ts) <> text " arguments to constructor “" <> text id' <> text "”, but got " <> text (show $ length args) <> dot <> linebreak)

                    (ts', cons, env) <- third mconcat <$> mapAndUnzip3M inferPattern args

                    let cons' = zip ts ts'

                    pure (r, cons' <> mconcat cons, env)
                  where lookupCtor :: String -> Infer Scheme
                        lookupCtor id' = do
                            env <- asks (getMap . ctorCtx)
                            case Map.lookup id' env of
                                Nothing -> throwError $ makeUnboundVarError id'
                                Just x  -> pure x

                        unfoldParams :: Type -> ([Type], Type)
                        unfoldParams (TFun a b) = first (a:) (unfoldParams b)
                        unfoldParams t = ([], t)

            third :: (c -> d) -> (a, b, c) -> (a, b, d)
            third f (a, b, c) = (a, b, f c)

            zipFrom :: a -> [b] -> [(a, b)]
            zipFrom = zip . repeat
            

inferTop :: GlobalEnv -> [(String, Expr)] -> Either TIError GlobalEnv
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

    fv (TVar a)   = [a]
    fv (TFun a b) = fv a <> fv b
    fv (TApp a b) = fv a <> fv b
    fv (TTuple e) = foldl (\acc t -> acc <> fv t) [] e
    fv _          = []

    normtype (TFun a b)       = TFun (normtype a) (normtype b)
    normtype (TApp a b)       = TApp (normtype a) (normtype b)
    normtype (TTuple e)       = TTuple (map normtype e)
    normtype (TVar a@(TV x')) =
        case Prelude.lookup a ord of
            Just x -> TVar x
            Nothing -> error $ "The type variable “" <> x' <> "” has not been declared in type, but wants to be used.\n"
                                <> "This error should never happen. If you see it, please report it to one of the maintainer of Blob."
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
unifyMany t1 t2 = throwError $ foldl (\acc (t1', t2') -> acc <> makeUnifyError t1' t2') empty (zip t1 t2)

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2          = pure nullSubst
unifies (TVar v)     t            = v `bind` t
unifies t            (TVar v    ) = v `bind` t
unifies (TFun t1 t2) (TFun t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies (TTuple e) (TTuple e')    = unifyMany e e'
unifies (TApp t1 t2) (TApp t3 t4) = unifyMany [t1, t2] [t3, t4]
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