{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Nihil.TypeChecking.Constraint where

import Nihil.TypeChecking.Core
import Nihil.TypeChecking.Substitution
import qualified Data.Set as Set

-- | Unification constraints for types
data TypeConstraint
    = Type :>~ Type     -- ^ Type equality
  deriving Show

data ClassConstraint
    = Implements Type
  deriving Show

-- | Unification constraints for kinds
data KindConstraint
    = Kind :*~ Kind     -- ^ Kind equality
  deriving Show

instance Substitutable TypeConstraint where
    type Subst TypeConstraint = Subst Type

    free (t1 :>~ t2) = free t1 `Set.union` free t2

    apply s (t1 :>~ t2) = apply s t1 :>~ apply s t2

instance Substitutable KindConstraint where
    type Subst KindConstraint = Subst Kind

    free (k1 :*~ k2) = free k1 `Set.union` free k2

    apply s (k1 :*~ k2) = apply s k1 :*~ apply s k2

instance Substitutable ClassConstraint where
    type Subst ClassConstraint = Subst Type

    free (Implements ty) = free ty

    apply s (Implements ty) = Implements (apply s ty)