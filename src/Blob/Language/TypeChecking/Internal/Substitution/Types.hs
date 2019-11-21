-- Blobc, a compiler for compiling Blob source code
-- Copyright (c) 2019 Mesabloo

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeApplications
           , FlexibleContexts #-}

module Blob.Language.TypeChecking.Internal.Substitution.Types where

import Blob.Language.TypeChecking.Internal.Type
import Blob.Language.TypeChecking.Internal.Substitution
import Blob.Language.TypeChecking.Internal.Environment (TypeEnv(..), _TypeEnv)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Lens (makePrisms, (^.), (%~))
import Data.Maybe (fromMaybe)
import Data.List (nub)
import Control.Monad (replicateM)
import Data.Bifunctor (first)
import Data.Composition ((.:))

newtype TypeSubst = Subst (Map.Map TVar Type)

makePrisms ''TypeSubst

unpack :: TypeSubst -> Map.Map TVar Type
unpack = (^. _Subst)
{-# INLINE unpack #-}

instance Substitutable TypeSubst Type TVar where
    fv (TVar n)            = Set.singleton n
    fv ((t1, _) `TFun` t2) = fv @TypeSubst t1 `Set.union` fv @TypeSubst t2
    fv (TTuple ts)         = foldr (Set.union . fv @TypeSubst) mempty ts
    fv (TApp t1 t2)        = fv @TypeSubst t1 `Set.union` fv @TypeSubst t2
    fv _                   = mempty

    apply s (TVar n)          = fromMaybe (TVar n) (Map.lookup n (unpack s))
    apply s (TFun (t1, l) t2) = (apply @_ @_ @TVar s t1, l) `TFun` apply @_ @_ @TVar s t2
    apply s (TTuple ts)       = TTuple (apply @_ @_ @TVar s <$> ts)
    apply s (TApp t1 t2)      = apply @_ @_ @TVar s t1 `TApp` apply @_ @_ @TVar s t2
    apply _ t                 = t

instance Semigroup TypeSubst where
    s1 <> s2 = Subst $ Map.map (apply @_ @_ @TVar s1) (unpack s2) `Map.union` unpack s1

instance Monoid TypeSubst where
    mempty = Subst mempty

-- | An infinite stream of letters, looping on the alphabet.
letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

-- | Gives fresh new rigid type variables to a 'Scheme'.
normalize :: Scheme -> Scheme
normalize (Scheme _ body) = Scheme (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (map TV letters)

    fv (TVar a)    = [a]
    fv (TFun a b)  = fv (fst a) <> fv b
    fv (TApp a b)  = fv a <> fv b
    fv (TTuple e)  = foldMap fv e
    fv _           = []

    normtype (TFun a b)       = TFun (first normtype a) (normtype b)
    normtype (TApp a b)       = TApp (normtype a) (normtype b)
    normtype (TTuple e)       = TTuple (map normtype e)
    normtype (TVar a@(TV x')) =
        case Prelude.lookup a ord of
            Just x -> TRigid x
            Nothing -> error $ "The type variable \"" <> x' <> "\" has not been declared in type, but wants to be used.\n"
                                <> "This error should never happen. If you see it, please report it."
    normtype t          = t

-- | Creates a 'Scheme' from a 'Type' by putting all the free type variables from the 'Type' into the 'Scheme'.
generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme as t
  where as = Set.toList $ fv @TypeSubst t `Set.difference` fv @TypeSubst env

-- | Canonicalizes and returns the polymorphic toplevel type.
closeOver :: Type -> Scheme
closeOver = normalize . generalize mempty

instance Substitutable TypeSubst TypeEnv TVar where
    fv    = fv @TypeSubst . Map.elems . (^. _TypeEnv)
    apply = TypeEnv .: flip (flip (Map.map . apply @_ @_ @TVar) . (^. _TypeEnv))

instance Substitutable TypeSubst a TVar => Substitutable TypeSubst [a] TVar where
    fv    = foldr (Set.union . fv @TypeSubst) mempty
    apply = fmap . apply @_ @_ @TVar

instance Substitutable TypeSubst Scheme TVar where
    fv (Scheme vars t)      = fv @TypeSubst t Set.\\ Set.fromList vars
    apply s (Scheme vars t) = Scheme vars (apply @_ @_ @TVar (foldr ((_Subst %~) . Map.delete) s vars) t)
