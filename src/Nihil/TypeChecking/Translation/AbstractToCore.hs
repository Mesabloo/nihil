{-# LANGUAGE ViewPatterns #-}

module Nihil.TypeChecking.Translation.AbstractToCore where

import qualified Nihil.Syntax.Abstract as AC
import Nihil.TypeChecking.Core
import Nihil.Utils.Source
import Nihil.Utils.Impossible
import Nihil.Utils.Annotation (hoistAnnotated)
import Data.Bifunctor (first)
import qualified Data.Map as Map

-- | Converts a desugared 'AC.Type' into a 'Type' understandable by the typechecker.
coerceType :: AC.Type -> Type
coerceType = hoistAnnotated f
  where
    f = first g
    g (AC.TId  i            ) = TId i
    g (AC.TVar v            ) = TRigid v
    g (AC.TApplication t1 t2) = TApplication (coerceType t1) (coerceType t2)
    g (AC.TTuple ts         ) = TTuple (coerceType <$> ts)
    g (AC.TRecord row)        = TRecord (coerceType row)
    g (AC.TRow stts rest)     = TRow (Map.fromList (coerceStatement <$> stts)) rest
      where coerceStatement (annotated -> AC.FunctionDeclaration name ty) = (name, coerceType ty)
            coerceStatement _ = impossible "Records can't hold any statement other than function declarations!"

-- | Converts a desugared 'AC.Scheme' (@∀ vs. ty@) into a 'Scheme' understandable by the typechecker.
coerceScheme :: AC.Scheme -> Scheme Type
coerceScheme (AC.Forall tv t) = Forall tv (coerceType t)

-- | Converts a 'AC.CustomType' into a 'CustomType' understandable by the typechecker.
coerceCustomType :: Scheme t -> AC.CustomType -> CustomType
coerceCustomType (Forall vars _) = hoistAnnotated (first (Forall vars . f))
  where f (AC.SumType ctors) = GADT (coerceScheme <$> ctors)
        f (AC.TypeAlias ty)  = TypeAlias (coerceType ty)
