{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Nihil.TypeChecking.Rules.Inference.Type
( inferExpr
, inferFunctionDefinition
, inferInstanceHead, inferInstanceBody
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
import qualified Nihil.Syntax.Abstract.Core as AC
import Nihil.TypeChecking.Errors.UnboundName
import Nihil.TypeChecking.Rules.Inference
import Control.Arrow ((&&&))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (local)
import Control.Lens (use, (+=), views, (%~), view, scribe)
import Prelude hiding (lookup, log)
import Control.Applicative ((<|>))
import Control.Monad (guard, mapAndUnzipM, forM)
import qualified Data.Map as Map
import Data.Function ((&))
import Data.Bifunctor (first, bimap)
import Data.Align (align)
import Data.These
import qualified Data.Map.Unordered as UMap
import Data.List (partition)

-- | Infers the type of a function definition (has a form of @f = e@) given its initial position, its name,
--   its value.
inferFunctionDefinition :: SourcePos -> String -> AC.Expr -> InferType Type
inferFunctionDefinition pos name ex = do
    tv  <- fresh "$" pos
    fty <- inEnvMany [(name, Forall [] tv)] (inferExpr ex)
    scribe typeConstraint [fty :>~ tv]

    funDefCtx `views` lookup name >>= \case
        Nothing           -> impossible "All functions must have types"
        Just (Forall _ t) -> do
            scribe typeConstraint [t :>~ fty]

    pure fty

inferInstanceHead :: String -> Type -> InferType ()
inferInstanceHead name ty =
    customTypeCtx `views` lookup name >>= \case
        Nothing -> impossible ("Typeclass " <> name <> " is forced to exist! It already has been filtered out!")
        Just (annotated -> Forall _ (Class head _)) -> scribe typeConstraint [relax head :>~ ty]
        Just _ -> impossible "A custom type cannot have the Constraint kind"

inferInstanceBody :: Scheme Type -> Scheme Type
inferInstanceBody (Forall _ ty) = Forall [] (relax ty)

-- | Infers the 'Type' of an 'AC.Expr'ession.
inferExpr :: AC.Expr -> InferType Type
inferExpr e =
    let (loc, ann) = (location &&& annotated) e
    in loc & case ann of
        AC.ELiteral lit       -> inferELiteral lit
        AC.ETypeHole          -> inferETypeHole
        AC.EId i              -> inferEId i
        AC.EApplication e1 e2 -> inferEApplication e1 e2
        AC.ETuple es          -> inferETuple es
        AC.ETypeAnnotated e t -> inferETypeAnnotated e t
        AC.ELambda pat ex     -> inferELambda pat ex
        AC.EMatch ex branches -> inferEMatch ex branches
        AC.ELet stts ex       -> inferELet stts ex

-- | Infers the type of an expression 'AC.Literal'.
inferELiteral :: AC.Literal -> SourcePos -> InferType Type
inferELiteral (AC.LInteger _) pos   = pure (locate (TId "Integer") pos)
inferELiteral (AC.LFloat _) pos     = pure (locate (TId "Double") pos)
inferELiteral (AC.LCharacter _) pos = pure (locate (TId "Char") pos)

-- | Generates a constraint for a type hole.
inferETypeHole :: SourcePos -> InferType Type
inferETypeHole pos = do
    hole <- fresh "_" pos
    ty   <- fresh "$" pos
    scribe typeConstraint [hole :>~ ty]
    pure hole

-- | Infers the type of an identifier.
inferEId :: String -> SourcePos -> InferType Type
inferEId n pos = do
    funEnv  <- funDefCtx `views` lookup n
    ctorEnv <- constructorCtx `views` lookup n
    maybe (throwError (unboundName (locate n pos))) (instantiate pos) (funEnv <|> ctorEnv)

-- | Infers the type of an expression application.
inferEApplication :: AC.Expr -> AC.Expr -> SourcePos -> InferType Type
inferEApplication e1 e2 pos = do
    ty1 <- inferExpr e1
    ty2 <- inferExpr e2
    tyv <- fresh "$" pos
    scribe typeConstraint [ty1 :>~ tFun ty2 tyv pos]
    pure tyv

-- | Infers the type of a tuple.
inferETuple :: [AC.Expr] -> SourcePos -> InferType Type
inferETuple es pos = do
    ts <- mapM inferExpr es
    pure (locate (TTuple ts) pos)

-- | Infers the type of an expression and generates a constraint for its annotation.
inferETypeAnnotated :: AC.Expr -> AC.Type -> SourcePos -> InferType Type
inferETypeAnnotated expr ty pos = do
    exTy <- inferExpr expr
    scribe typeConstraint [exTy :>~ coerceType ty]
    pure exTy

-- | Infers the type of a lambda abstraction.
inferELambda :: AC.Pattern -> AC.Expr -> SourcePos -> InferType Type
inferELambda pat ex pos = do
    (patTy, env) <- inferPattern pat
    ty           <- inEnvMany (Map.toList env) (inferExpr ex)
    pure (tFun patTy ty pos)

-- | Infers the type of a pattern matching expression.
inferEMatch :: AC.Expr -> [(AC.Pattern, AC.Expr)] -> SourcePos -> InferType Type
inferEMatch ex branches pos = do
    ty     <- inferExpr ex
    result <- unzip <$> forM branches \(pat, expr) -> do
        (pTy, env) <- inferPattern pat
        exTy       <- inEnvMany (Map.toList env) (inferExpr expr)
        pure (exTy, pTy)
    let (ret:xs, patsTy) = result

    let constraints = (uncurry (:>~) <$> (zipFrom ret xs <> zipFrom ty patsTy))
    scribe typeConstraint constraints
    pure ret
  where zipFrom = zip . repeat

-- | Infers the type of a @let@ expression.
inferELet :: [AC.Statement] -> AC.Expr -> SourcePos -> InferType Type
inferELet stts ex pos = do
    env <- UMap.traverseWithKey inferStmt (uncurry align (sepStatements (annotated <$> stts)))
    ty  <- inEnvMany (UMap.toList env) (inferExpr ex)
    pure ty
  where inferStmt name (That def)       = do
            tv <- fresh "$" pos
            ty <- inEnvMany [(name, Forall [] tv)] (inferExpr def)
            scribe typeConstraint [ty :>~ tv]
            env <- view funDefCtx
            pure (generalize env ty)
        inferStmt name (These decl def) = do
            scheme@(Forall _ ty) <- inferStmt name (That def)
            scribe typeConstraint [ty :>~ coerceType decl]
            pure scheme
        inferStmt name (This _)         =
            throwError (lacksBind (locate name pos))

        sepStatements = bimap (UMap.fromList . fmap toDecl)
                              (UMap.fromList . fmap toDef) . partition f

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

type PatternEnv = (Type, Map.Map String (Scheme Type))

-- | Infers the type and environment of a pattern
inferPattern :: AC.Pattern -> InferType PatternEnv
inferPattern pat =
    let (loc, ann) = (location &&& annotated) pat
    in loc & case ann of
        AC.PWildcard          -> inferPWildcard
        AC.PLiteral lit       -> inferPLiteral lit
        AC.PId i              -> inferPId i
        AC.PTuple ps          -> inferPTuple ps
        AC.PTypeAnnotated p t -> inferPTypeAnnotated p t
        AC.PConstructor n ps  -> inferPConstructor n ps

-- | Infers the type of a pattern wildcard.
inferPWildcard :: SourcePos -> InferType PatternEnv
inferPWildcard pos = (, mempty) <$> fresh "$" pos

-- | Infers the type of a pattern literal.
inferPLiteral :: AC.Literal -> SourcePos -> InferType PatternEnv
inferPLiteral lit pos = (, mempty) <$> inferELiteral lit pos

-- | Infers the type of an identifier.
inferPId :: String -> SourcePos -> InferType PatternEnv
inferPId n pos = do
    ty <- fresh "$" pos
    pure (ty, Map.singleton n (Forall [] ty))

-- | Infers the type of a pattern tuple
inferPTuple :: [AC.Pattern] -> SourcePos -> InferType PatternEnv
inferPTuple ps pos = do
    (ts, envs) <- unzip <$> mapM inferPattern ps
    pure (locate (TTuple ts) pos, mconcat envs)

-- | Infers the type of an expression and generates a contraint for its type.
inferPTypeAnnotated :: AC.Pattern -> AC.Type -> SourcePos -> InferType PatternEnv
inferPTypeAnnotated pat ty pos = do
    (pty, env) <- inferPattern pat
    let ty' = coerceType ty
    scribe typeConstraint [ty' :>~ pty]
    pure (ty', env)

-- | Infers the type of a pattern data constructor.
inferPConstructor :: String -> [AC.Pattern] -> SourcePos -> InferType PatternEnv
inferPConstructor n ps pos = do
    constructor <- instantiate pos =<< lookupCtor n
    let (tys, ret) = foldParams constructor

    guard (length ps == length tys)
        <|> throwError (missingConstructorArgument (locate n pos) (length tys) (length ps))

    (tys', env) <- fmap mconcat <$> mapAndUnzipM inferPattern ps
    scribe typeConstraint (uncurry (:>~) <$> zip tys tys')
    pure (ret, env)
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
        f (TImplements t1 t2)  = TImplements (relax t1) (relax t2)
        f t                    = t

inEnvMany :: [(String, Scheme Type)] -> InferType a -> InferType a
inEnvMany env = local (funDefCtx %~ union (Env (Map.fromList env)))
