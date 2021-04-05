{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Nihil.TypeChecking.Substitution where

import Nihil.Utils.Source
import Nihil.Utils.Annotation
import Nihil.TypeChecking.Core
import qualified Data.Set as Set
import Control.Arrow ((>>^))
import Data.Bifunctor (first)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Foldable (fold)

newtype Subst' a = Subst (Map.Map String a)
  deriving
    ( -- | Use only for debugging
      Show )

instance (Substitutable a, Subst a ~ Subst' a) => Semigroup (Subst' a) where
    s1@(Subst s1') <> (Subst s2') = Subst (Map.map (apply s1) s2' `Map.union` s1')

instance (Substitutable a, Subst a ~ Subst' a) => Monoid (Subst' a) where
    mempty = Subst mempty

{-| A type class for substitutable values. -}
class Monoid (Subst a) => Substitutable a where
    type Subst a

    -- | Applies a substitution to a substitutable value.
    apply :: Subst a -> a -> a

    -- | Gathers all the free variables from a substitutable value.
    free  :: a -> Set.Set String

instance Substitutable a => Substitutable [a] where
    type Subst [a] = Subst a

    free  = foldr (Set.union . free) mempty
    apply = fmap . apply

instance Substitutable a => Substitutable (Located a) where
    type Subst (Located a) = Subst a

    free = annotated >>^ free
    apply s = hoistAnnotated (first (apply s))

-------------------------------------------------------------------------

instance Substitutable Kind where
    type Subst Kind = Subst' Kind

    free (KVar v)             = Set.singleton v
    free (KApplication k1 k2) = mconcat (free <$> [k1, k2])
    free _                    = mempty

    apply (Subst sub) kv@(KVar v) = fromMaybe kv (Map.lookup v sub)
    apply s (KApplication k1 k2)  = KApplication (apply s k1) (apply s k2)
    apply _ k                     = k

instance Substitutable Type' where
    type Subst Type' = Subst' Type'

    free (TVar v)             = Set.singleton v
    free (TTuple ts)          = free ts
    free (TApplication t1 t2) = free [t1, t2]
    free (TRow funs ty)       = fold (free <$> funs) <> maybe mempty free ty
    free (TRecord row)        = free row
    free _                    = mempty

    apply (Subst sub) tv@(TVar v) = fromMaybe tv (Map.lookup v sub)
    apply s (TTuple ts)           = TTuple (apply s ts)
    apply s (TApplication t1 t2)  = TApplication (apply s t1) (apply s t2)
    apply s (TRow ss r)           = TRow (apply s <$> ss) (apply s <$> r)
    apply s (TRecord row)         = TRecord (apply s row)
    apply _ t                     = t

instance (Substitutable a, Subst a ~ Subst' b) => Substitutable (Scheme a) where
    type Subst (Scheme a) = Subst a

    free (Forall v t) = free t Set.\\ Set.fromList v

    apply (Subst s) (Forall v t) = Forall v (apply newSubst t)
      where newSubst = Subst (Map.withoutKeys s (Set.fromList v))
