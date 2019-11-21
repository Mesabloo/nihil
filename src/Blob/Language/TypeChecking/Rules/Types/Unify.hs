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

{-# LANGUAGE LambdaCase, FlexibleInstances, MultiParamTypeClasses
           , FlexibleContexts, TypeApplications #-}

module Blob.Language.TypeChecking.Rules.Types.Unify where

import Blob.Language.TypeChecking.Internal.Type
import Blob.Language.TypeChecking.Internal.Substitution.Types
import Blob.Language.TypeChecking.TypeChecker (Solve)
import Blob.Language.TypeChecking.Internal.Errors.Unification
import Blob.Language.TypeChecking.Internal.Errors.InfiniteType
import Blob.Language.TypeChecking.Internal.Errors.TypeHole
import Blob.Language.TypeChecking.Internal.Unification
import Blob.Language.TypeChecking.Internal.Environment (typeDefCtx)
import Blob.Language.TypeChecking.Internal.Substitution
import Blob.Language.PrettyPrinting.Types ()
import Control.Monad.Except (throwError)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Lens (views)

instance Unifiable Type TypeSubst Solve where
    -- | Unifies two types into a single substitution.
    unify t1 t2 | t1 == t2          = pure mempty
    unify (TVar v) t                = v `bind` t
    unify t            (TVar v    ) = v `bind` t
    unify (TId i) t       = unifyAlias i t
    unify t       (TId i) = unifyAlias i t
    unify (TFun (t1, l1) t2) (TFun (t3, l2) t4) = unifyMany [t1, t2] [t3, t4]
    unify (TTuple e) (TTuple e')    = unifyMany e e'
    unify (TApp t1 t2) (TApp t3 t4) = unifyMany [t1, t2] [t3, t4]
    unify a@(TApp _ _) t = unifyCustom a t
    unify t a@(TApp _ _) = unifyCustom a t
    unify t1           t2           =
        let (Scheme _ st1) = closeOver t1
            (Scheme _ st2) = closeOver t2
        in throwError $ makeUnifyError st1 st2

-- | Unifies a type application, searching in the environment for custom types.
unifyCustom :: Type -> Type -> Solve TypeSubst
unifyCustom a@(TApp t1 t2) t3 = go t1 [t2]
  where go (TApp t1' t2') args = go t1' (t2':args)
        go (TId i) args = typeDefCtx `views` Map.lookup i >>= \case
            Just (CustomScheme tvs (TAlias t)) ->
                let sub = Subst $ Map.fromList (zipWith (\k v -> (TV k, v)) tvs args)
                in unify (apply @_ @_ @TVar sub t) t3
            Just _ ->
                let (Scheme _ st3) = closeOver t3
                in throwError $ makeUnifyError a st3
            Nothing -> undefined -- ? case handled by kind checking
        go _ _ = let (Scheme _ st3) = closeOver t3
                 in throwError $ makeUnifyError a st3
unifyCustom _ _ = undefined -- ! never happening

-- | Unifies a type with a type alias, searching in the environment.
unifyAlias :: String -> Type -> Solve TypeSubst
unifyAlias name t1 =
    typeDefCtx `views` Map.lookup name >>= \case
        Just (CustomScheme _ (TAlias t2)) -> unify t2 t1
        Just _ ->
            let (Scheme _ st1) = closeOver t1
            in throwError $ makeUnifyError (TId name) st1
        Nothing ->
            let (Scheme _ st1) = closeOver t1
            in throwError $ makeUnifyError (TId name) st1 -- ? Should never happen

-- | Binds a type variable to a type, checking for infinite types.
bind :: TVar -> Type -> Solve TypeSubst
bind a t | t == TVar a     = case a of
                    TV (h:_) | h == '_' ->
                        let (Scheme _ st) = closeOver t
                        in throwError $ makeHoleError st
                    _ -> pure mempty
         | occursCheck a t = throwError $ makeInfiniteTypeError a t
         | otherwise       = pure (Subst $ Map.singleton a t)
  where
    -- | Checks whether we formed an infinite type of not.
    occursCheck :: TVar -> Type -> Bool
    occursCheck a t = a `Set.member` fv @TypeSubst t
