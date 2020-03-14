{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Nihil.TypeChecking.Core
( Kind(..), Type, Type'(..)
, Scheme(..)
, CustomType, CustomType'(..)
, Subst'(..) ) where

import Nihil.Utils.Source
import Nihil.TypeChecking.Substitution
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Foldable (fold)

data Kind
    = KStar                   -- ^ > { * }
    | KVar String             -- ^ > { k }
    | KApplication Kind Kind  -- ^ > { k₁ k₂ }
    | KArrow                  -- ^ > { -> } or { → }
    | KRow                    -- ^ A special kind for row types
  deriving
    ( -- | Use only for debugging
      Show
    , Eq )

instance Substitutable Kind where
    type Subst Kind = Subst' Kind

    free (KVar v)             = Set.singleton v
    free (KApplication k1 k2) = mconcat (free <$> [k1, k2])
    free _                    = mempty

    apply (Subst sub) kv@(KVar v) = fromMaybe kv (Map.lookup v sub)
    apply s (KApplication k1 k2)  = KApplication (apply s k1) (apply s k2)
    apply _ k                     = k

type Type = Located Type'
data Type'
    = TId String              -- ^ > { Type }
    | TVar String             -- ^ > { a }
    | TRigid String           -- ^ > { a }
    | TTuple [Type]           -- ^ > { (a, b, c) }
    | TApplication Type Type  -- ^ > { t₁ t₂ }
    | TPrim String
    | TRow (Map.Map String Type) (Maybe Type)
                              -- ^ > { f : t1 ; g : t2 | rest }
    | TRecord Type            -- ^ > { { f : t1 ; g : t2 | rest } }
  deriving
    ( -- | Use only for debugging
      Show
    , Eq )

type CustomType = Located (Scheme CustomType')
data CustomType'
    = TypeAlias Type                        -- ^ > type T = { U a b }
    | GADT (Map.Map String (Scheme Type))   -- ^ > data X where { C : X }
  deriving
    ( -- | Use only for debugging
      Show
    , Eq )

data Scheme t = Forall [String] t   -- ^ > { ∀ a b. a b }
  deriving
    ( -- | Use only for debugging
      Show
    , Eq )

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
