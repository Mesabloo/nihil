{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

module Nihil.TypeChecking.Rules.Inference.Kind
( inferKind, inferScheme, inferCustomType, inferTypeClass ) where

import Nihil.TypeChecking.Core
import Nihil.TypeChecking.Common
import Nihil.Utils.Source
import Nihil.Utils.Impossible
import Nihil.TypeChecking.Environment
import Nihil.TypeChecking.Constraint
import Nihil.TypeChecking.Errors.UndefinedType
import Nihil.TypeChecking.Substitution
import Control.Arrow ((>>>))
import Prelude hiding (lookup, log)
import Control.Monad.Reader (asks, local)
import Control.Monad.Except (throwError)
import Control.Monad.Writer (tell)
import Control.Lens (use, (+=))
import qualified Data.Map as Map
import Control.Monad (forM)

-- | Infers the 'Kind' of a 'CustomType'.
inferCustomType :: SourcePos -> String -> Scheme CustomType -> InferKind Kind
inferCustomType pos name sc@(Forall tvs _) = do
    args <- forM tvs (const (fresh "$"))
    let kind = foldr kArr KStar args
    k    <- local (`extend` (name, kind)) (inferCustomTypeScheme sc)
    tell [kind :*~ k]
    pure kind

-- | Infers the 'Kind' of a 'Type' and generates some 'KindConstraint's if needed.
inferKind :: Type -> InferKind Kind
inferKind = annotated >>> f
  where f :: Type' -> InferKind Kind
        f (TId n) =
            asks (lookup n) >>= maybe (throwError (undefinedType n)) pure
        f (TVar n) =
            asks (lookup n) >>= maybe (throwError (undefinedType n)) pure
        f (TRigid n) =
            asks (lookup n) >>= maybe (throwError (undefinedType n)) pure
        f (TTuple ts) = do
            mapM_ inferKind ts
            pure KStar
        f (TApplication t1 t2) = do
            k1 <- inferKind t1
            k2 <- inferKind t2
            kv <- fresh "$"
            tell [k1 :*~ kArr k2 kv]
            pure kv
        f (TPrim _) = pure KStar
        f (TImplements t1 t2) = do
            k1 <- inferKind t1
            k2 <- inferKind t2
            tell [k1 :*~ KConstraint, k2 :*~ KStar]
            pure KStar

-- | Infers the kind of a typeclass
inferTypeClass :: String -> Scheme Type -> [Scheme Type] -> InferKind Kind
inferTypeClass name (Forall tvs classTy) funs = do
    typeArgs <- Env . Map.fromList <$> mapM ((<$> fresh "$") . (,)) tvs
    kv <- fresh "$"

    let kind = foldl1 kArr typeArgs
    _ <- local (union (Env (Map.fromList [(name, kv)]))) do
        local (union typeArgs) (mapM inferScheme funs)

    pure (kArr kind KConstraint)

-- | Infers the kind of a generalized 'Type'.
inferScheme :: Scheme Type -> InferKind Kind
inferScheme (Forall vars ty) = do
    newVars <- Env . Map.fromList <$> mapM ((<$> fresh "$") . (,)) vars
    local (union newVars) (inferKind ty)

inferCustomTypeScheme :: Scheme CustomType -> InferKind Kind
inferCustomTypeScheme (Forall vars cty) = do
    typeArgs <- Env . Map.fromList <$> mapM ((<$> fresh "$") . (,)) vars
    kind     <- local (union typeArgs) case annotated cty of
        Forall _ (GADT ctors)   -> KStar <$ inferConstrs (Map.toList ctors)
        Forall _ (TypeAlias ty) -> inferKind ty
        Forall _ (Class _ _)    -> impossible "Type classes are kind checked on their own."
    let kind' = foldr kArr kind ((typeArgs `at`) <$> vars)
    pure kind'

inferConstrs :: [(String, Scheme Type)] -> InferKind ()
inferConstrs []                   = pure ()
inferConstrs ((_, Forall _ c):cs) = do
    inferConstructor (foldConstructor (annotated c))
    inferConstrs cs
  where foldConstructor (TApplication arrow t2) =
            case annotated arrow of
                TApplication arr t1
                    | annotated arr == TId "->" -> t1 : foldConstructor (annotated t2)
                    | annotated arr == TId "→"  -> t1 : foldConstructor (annotated t2)
                _                               -> []
        foldConstructor _                       = []

-- | Generates 'KindConstraint's for a data type constructor.
inferConstructor :: [Type] -> InferKind ()
inferConstructor [] = pure ()
inferConstructor (t:ts) = do
    kind <- inferKind t
    tell [kind :*~ KStar]
    inferConstructor ts

kArr :: Kind -> Kind -> Kind
kArr = KApplication . KApplication KArrow

-- | Generates a fresh new 'Kind' from a prefix.
fresh :: String -> InferKind Kind
fresh n = do
    s <- use supply
    supply += 1

    env <- asks free
    let new = n <> show s

    if new `notElem` env
    then pure (KVar new)
    else fresh new