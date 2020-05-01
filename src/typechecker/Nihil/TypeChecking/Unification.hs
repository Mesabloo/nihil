{-# LANGUAGE MultiParamTypeClasses #-}

module Nihil.TypeChecking.Unification where

import Nihil.TypeChecking.Substitution
import Nihil.Utils.Impossible
import Nihil.TypeChecking.Common

{-| A type class for unifiable values.

    Unifiable values include ─ but are not limited to ─ types and kinds.

    The @'unify'@ function represents the @~@ operator commonly used to denote equality.
-}
class (Show a, Substitutable a) => Unifiable a e where
    -- | Tries to unify two unifiable values (most likely to be kinds or types).
    --
    --   May fail because @'Solve'@ is a @'Control.Monad.Except.MonadError'@.
    unify :: a -> a -> Solve e (Subst a)

    -- | Unifies multiple values, effectively applying the substitution between each unification.
    unifyMany :: [a] -> [a] -> Solve e (Subst a)
    unifyMany [] []             = pure mempty
    unifyMany (t1:ts1) (t2:ts2) = do
        sub  <- unify t1 t2
        sub' <- unifyMany (apply sub ts1) (apply sub ts2)
        pure (sub <> sub')
    unifyMany l r               =
        impossible ("Could not unify " <> show l <> " and " <> show r <> ": either one or the other isn't long enough!")