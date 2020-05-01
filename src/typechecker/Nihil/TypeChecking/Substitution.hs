{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Nihil.TypeChecking.Substitution where

import Nihil.Utils.Source
import Nihil.Utils.Annotation
import qualified Data.Set as Set
import Control.Arrow ((>>^))
import Data.Bifunctor (first)
import qualified Data.Map as Map

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