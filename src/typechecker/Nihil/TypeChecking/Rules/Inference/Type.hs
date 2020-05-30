{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Nihil.TypeChecking.Rules.Inference.Type
( elabExpr
, elabFunctionDefinition
, fresh ) where

import Nihil.TypeChecking.Core
import Nihil.TypeChecking.Environment
import Nihil.TypeChecking.Constraint
import Nihil.TypeChecking.Common
import Nihil.Utils.Source
import Nihil.Utils.Impossible
import Nihil.Utils.Annotation
import Nihil.TypeChecking.Substitution
import Nihil.TypeChecking.Translation.AbstractToCore (coerceType)
import Nihil.TypeChecking.Errors.MissingArgument
import Nihil.TypeChecking.Errors.BindLack
import qualified Nihil.Runtime.Core as RC
import qualified Nihil.Syntax.Abstract.Core as AC
import Nihil.TypeChecking.Errors.UnboundName
import Nihil.TypeChecking.Rules.Inference
import Control.Arrow ((&&&))
import Control.Monad.Writer (tell)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (local, asks)
import Control.Lens (use, (+=), views, (%~), view)
import Prelude hiding (lookup, log)
import Control.Applicative ((<|>))
import Control.Monad (guard, mapAndUnzipM, forM)
import qualified Data.Map as Map
import Data.Function ((&))
import Data.Bifunctor (first, bimap)
import Data.Align (align)
import Data.These
import Data.List (partition)

type ExprEnv = (RC.VExpr, Type)

-- | Infers the type of a function definition (has a form of @f = e@) given its initial position, its name,
--   its value.
elabFunctionDefinition :: SourcePos -> String -> AC.Expr -> InferType ExprEnv
elabFunctionDefinition pos name ex = do
    tv            <- fresh "$" pos
    (elabEx, fty) <- inEnvMany [(name, Forall [] tv)] (elabExpr ex)
    tell [fty :>~ tv]

    funDefCtx `views` lookup name >>= \case
        Nothing           -> impossible "All functions must have types"
        Just (Forall _ t) -> tell [t :>~ fty]

    pure (elabEx, fty)

-- | Infers the 'Type' of an 'AC.Expr'ession.
elabExpr :: AC.Expr -> InferType ExprEnv
elabExpr e =
    let (loc, ann) = (location &&& annotated) e
    in loc & case ann of
        AC.ELiteral lit       -> elabELiteral lit
        AC.ETypeHole          -> elabETypeHole
        AC.EId i              -> elabEId i
        AC.EApplication e1 e2 -> elabEApplication e1 e2
        AC.ETuple es          -> elabETuple es
        AC.ETypeAnnotated e t -> elabETypeAnnotated e t
        AC.ELambda pat ex     -> elabELambda pat ex
        AC.EMatch ex branches -> elabEMatch ex branches
        AC.ELet stts ex       -> elabELet stts ex

-- | Infers the type of an expression 'AC.Literal'.
elabELiteral :: AC.Literal -> SourcePos -> InferType ExprEnv
elabELiteral (AC.LInteger i) pos   = pure (RC.EInteger i, locate (TId "Integer") pos)
elabELiteral (AC.LFloat f) pos     = pure (RC.EDouble f, locate (TId "Double") pos)
elabELiteral (AC.LCharacter c) pos = pure (RC.ECharacter c, locate (TId "Char") pos)

-- | Generates a constraint for a type hole.
elabETypeHole :: SourcePos -> InferType ExprEnv
elabETypeHole pos = do
    hole <- fresh "_" pos
    ty   <- fresh "$" pos
    tell [hole :>~ ty]
    pure (RC.ETypeHole, hole)

-- | Infers the type of an identifier.
elabEId :: String -> SourcePos -> InferType ExprEnv
elabEId n pos = do
    funEnv  <- funDefCtx `views` lookup n
    ctorEnv <- constructorCtx `views` lookup n
    ty      <- maybe (throwError (unboundName (locate n pos))) (instantiate pos) (funEnv <|> ctorEnv)
    pure (RC.EId n, ty)

-- | Infers the type of an expression application.
elabEApplication :: AC.Expr -> AC.Expr -> SourcePos -> InferType ExprEnv
elabEApplication e1 e2 pos = do
    (elabE1, ty1) <- elabExpr e1
    (elabE2, ty2) <- elabExpr e2
    tyv           <- fresh "$" pos
    tell [ty1 :>~ tFun ty2 tyv pos]
    pure (RC.EApplication elabE1 elabE2, tyv)

-- | Infers the type of a tuple.
elabETuple :: [AC.Expr] -> SourcePos -> InferType ExprEnv
elabETuple es pos = do
    (elabs, ts) <- unzip <$> mapM elabExpr es
    pure (RC.ETuple elabs, locate (TTuple ts) pos)

-- | Infers the type of an expression and generates a constraint for its annotation.
elabETypeAnnotated :: AC.Expr -> AC.Type -> SourcePos -> InferType ExprEnv
elabETypeAnnotated expr ty pos = do
    (elabE, exTy) <- elabExpr expr
    tell [exTy :>~ coerceType ty]
    pure (elabE, exTy)

-- | Infers the type of a lambda abstraction.
elabELambda :: AC.Pattern -> AC.Expr -> SourcePos -> InferType ExprEnv
elabELambda pat ex pos = do
    ((elabP, patTy), env) <- elabPattern pat
    (elabEx, ty)          <- inEnvMany (Map.toList env) (elabExpr ex)
    pure (RC.ELambda elabP elabEx, tFun patTy ty pos)

-- | Infers the type of a pattern matching expression.
elabEMatch :: AC.Expr -> [(AC.Pattern, AC.Expr)] -> SourcePos -> InferType ExprEnv
elabEMatch ex branches pos = do
    (elabEx, ty) <- elabExpr ex
    (elabs, tys) <- unzip <$> forM branches \(pat, expr) -> do
        ((elabP, pTy), env) <- elabPattern pat
        (elabEx, exTy)      <- inEnvMany (Map.toList env) (elabExpr expr)
        pure ((elabP, elabEx), (pTy, exTy))
    let ((_, ret):xs) = tys

    let constraints = (uncurry (:>~) <$> (zipFrom ret (snd <$> xs) <> zipFrom ty (fst <$> tys)))
    tell constraints
    pure (RC.EMatch elabEx elabs, ret)
  where zipFrom = zip . repeat

-- | Infers the type of a @let@ expression.
elabELet :: [AC.Statement] -> AC.Expr -> SourcePos -> InferType ExprEnv
elabELet stts ex pos = do
    let allStts@(_, defs) = sepStatements (annotated <$> stts)
    env          <- Map.toList <$> Map.traverseWithKey check (uncurry align allStts)
    (stts, env)  <- inEnvMany env (traverseElabStatements (Map.toList defs))
    (elabEx, ty) <- local (const env) (elabExpr ex)
    pure (RC.ELet stts elabEx, ty)
  where traverseElabStatements []              = asks ([], )
        traverseElabStatements ((name, ex):ss) = do
            env <- view funDefCtx
            (elabEx, ty) <- elabFunctionDefinition pos name ex
            first ((name, elabEx) :) <$> inEnvMany [(name, generalize env ty)] (traverseElabStatements ss)

        check name (This _)     = throwError (lacksBind (locate name pos))
        check name (That _)     = Forall [] <$> fresh "$" pos
        check name (These ty _) = do
            env <- view funDefCtx
            pure (generalize env (coerceType ty))

        sepStatements = bimap (Map.fromList . fmap toDecl)
                              (Map.fromList . fmap toDef) . partition f

        f AC.FunctionDeclaration{} = True
        f AC.FunctionDefinition{}  = False
        f _                        =
            impossible "Let expression cannot contain type definitions."

        toDecl (AC.FunctionDeclaration name decl) = (name, decl)
        toDecl _ = impossible "Let expressions cannot contain type definitions."
        toDef  (AC.FunctionDefinition name def)   = (name, def)
        toDef  _ = impossible "Let expressions cannot contain type definitions."

tFun :: Type -> Type -> SourcePos -> Type
tFun t1 t2 pos = locate (TApplication (locate tApp pos) t2) pos
  where tApp = TApplication (locate (TId "->") pos) t1

---------------------------------------------------------------------------------------------------

type PatternEnv = ((RC.VPattern, Type), Map.Map String (Scheme Type))

-- | Infers the type and environment of a pattern
elabPattern :: AC.Pattern -> InferType PatternEnv
elabPattern pat =
    let (loc, ann) = (location &&& annotated) pat
    in loc & case ann of
        AC.PWildcard          -> elabPWildcard
        AC.PLiteral lit       -> elabPLiteral lit
        AC.PId i              -> elabPId i
        AC.PTuple ps          -> elabPTuple ps
        AC.PTypeAnnotated p t -> elabPTypeAnnotated p t
        AC.PConstructor n ps  -> elabPConstructor n ps

-- | Infers the type of a pattern wildcard.
elabPWildcard :: SourcePos -> InferType PatternEnv
elabPWildcard pos = (, mempty) . (RC.PWildcard, ) <$> fresh "$" pos

-- | Infers the type of a pattern literal.
elabPLiteral :: AC.Literal -> SourcePos -> InferType PatternEnv
elabPLiteral (AC.LInteger i) pos   = pure ((RC.PInteger i, locate (TId "Integer") pos), mempty)
elabPLiteral (AC.LFloat f) pos     = pure ((RC.PDouble f, locate (TId "Double") pos), mempty)
elabPLiteral (AC.LCharacter c) pos = pure ((RC.PCharacter c, locate (TId "Char") pos), mempty)

-- | Infers the type of an identifier.
elabPId :: String -> SourcePos -> InferType PatternEnv
elabPId n pos = do
    ty <- fresh "$" pos
    pure ((RC.PId n, ty), Map.singleton n (Forall [] ty))

-- | Infers the type of a pattern tuple
elabPTuple :: [AC.Pattern] -> SourcePos -> InferType PatternEnv
elabPTuple ps pos = do
    ((pats, ts), envs) <- first unzip . unzip <$> mapM elabPattern ps
    pure ((RC.PTuple pats, locate (TTuple ts) pos), mconcat envs)

-- | Infers the type of an expression and generates a contraint for its type.
elabPTypeAnnotated :: AC.Pattern -> AC.Type -> SourcePos -> InferType PatternEnv
elabPTypeAnnotated pat ty pos = do
    ((pex, pty), env) <- elabPattern pat
    let ty' = coerceType ty
    tell [ty' :>~ pty]
    pure ((pex, ty'), env)

-- | Infers the type of a pattern data constructor.
elabPConstructor :: String -> [AC.Pattern] -> SourcePos -> InferType PatternEnv
elabPConstructor n ps pos = do
    constructor <- instantiate pos =<< lookupCtor n
    let (tys, ret) = foldParams constructor

    guard (length ps == length tys)
        <|> throwError (missingConstructorArgument (locate n pos) (length tys) (length ps))

    ((elabPs, tys'), env) <- first unzip . fmap mconcat <$> mapAndUnzipM elabPattern ps
    tell (uncurry (:>~) <$> zip tys tys')
    pure ((RC.PConstructor n elabPs, ret), env)
  where lookupCtor :: String -> InferType (Scheme Type)
        lookupCtor name = do
            env <- constructorCtx `views` lookup name
            maybe (throwError (unboundName (locate name pos))) pure env

foldParams :: Type -> ([Type], Type)
foldParams t = case annotated t of
    TApplication t1 t2 -> case annotated t1 of
        TApplication t3 t4
          | annotated t3 == TId "->" -> first (t4 :) (foldParams t2)
          | annotated t3 == TId "→"  -> first (t4 :) (foldParams t2)
        _                            -> ([], t)
    _                  -> ([], t)


---------------------------------------------------------------------------------------------------

-- | Generates a fresh new type variable from the given prefix.
fresh :: String -> SourcePos -> InferType Type
fresh t pos = do
    n <- use supply
    supply += 1

    env <- funDefCtx `views` free
    let new = t <> show n

    if new `notElem` env
    then pure (locate (TVar new) pos)
    else fresh new pos

instantiate :: SourcePos -> Scheme Type -> InferType Type
instantiate pos (Forall vars ty) = do
    tvs <- fmap annotated <$> mapM (const (fresh "$" pos)) vars
    let sub = Subst (Map.fromList (zip vars tvs))
    pure (apply sub (relax ty))

-- | Relaxing a type means to transform rigid type variables into substitutable type variables.
relax :: Type -> Type
relax = hoistAnnotated (first f)
  where f (TApplication t1 t2) = TApplication (relax t1) (relax t2)
        f (TTuple ts)          = TTuple (relax <$> ts)
        f (TRigid v)           = TVar v
        f t                    = t

inEnvMany :: [(String, Scheme Type)] -> InferType a -> InferType a
inEnvMany env = local (funDefCtx %~ union (Env (Map.fromList env)))
