{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nihil.TypeChecking.Rules.Unification.Type
() where

import Nihil.TypeChecking.Unification
import Nihil.TypeChecking.Substitution
import Nihil.TypeChecking.Core
import Nihil.TypeChecking.Common
import Nihil.TypeChecking.Environment
import Nihil.Utils.Source
import Nihil.Utils.Impossible (impossible)
import Nihil.TypeChecking.Errors.Infinite (infiniteType)
import Nihil.TypeChecking.Errors.Unification (unifyType)
import Nihil.TypeChecking.Errors.TypeHole (typeHole)
import Nihil.TypeChecking.Errors.RecordDomainSubset (cannotSubtypeRecordDomains)
import Nihil.TypeChecking.Errors.MissingFieldsInRecord (missingFieldsInRecord)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Except (throwError)
import Data.List (isPrefixOf)
import Control.Monad (when, forM)
import Control.Lens (views)
import Prelude hiding (lookup)
import qualified Data.List as List

instance Unifiable Type GlobalEnv where
    unify t1 t2 = case (annotated t1, annotated t2) of
        (t1, t2) | t1 == t2                      -> pure mempty
        (TVar a, _)                              -> bind a t2
        (_, TVar a)                              -> bind a t1
        (TId i, _)                               -> unifyAlias t1 t2
        (_, TId i)                               -> unifyAlias t2 t1
        (TTuple e1, TTuple e2)                   -> unifyMany e1 e2
        (TApplication t1 t2, TApplication t3 t4) -> unifyMany [t1, t2] [t3, t4]
        (TApplication{}, _)                      -> unifyCustom t1 t2
        (_, TApplication{})                      -> unifyCustom t2 t1
        (TRecord row1, TRecord row2)             -> unify row1 row2
        (TRow f1 r1, TRow f2 r2)                 -> unifyRows (f1, r1) (f2, r2) (location t1)
        _                                        -> throwError (unifyType t1 t2)

unifyRows :: (Map.Map String Type, Maybe Type) -> (Map.Map String Type, Maybe Type) -> SourcePos -> SolveType (Subst Type)
unifyRows (f1, ext1) (f2, ext2) pos = do
    let dom1 = Map.keys f1
        dom2 = Map.keys f2
        commonFields = dom1 `List.intersect` dom2
        otherFields1 = dom1 List.\\ commonFields
        otherFields2 = dom2 List.\\ commonFields
    fieldSubst <- mconcat <$> forM commonFields \ k -> unify (f1 Map.! k) (f2 Map.! k)

    pure fieldSubst

-- | Unifies custom types and checks for well formed type applications.
unifyCustom :: Type -> Type -> SolveType (Subst Type)
unifyCustom t1 t2 = case annotated t1 of
    TApplication t1' t2' -> go t1' [t2']
    _                    -> impossible "Could not `unifyCustom` when base case is not a type application."
  where go :: Type -> [Type] -> SolveType (Subst Type)
        go t3 t4 = case annotated t3 of
            TApplication t1' t2' -> go t1' (t2' : t4)
            TId i                ->
                customTypeCtx `views` lookup i >>= \case
                    Just cty -> case annotated cty of
                        Forall tvs (TypeAlias t) ->
                            let sub = Subst (Map.fromList (zip tvs (fmap annotated t4)))
                            in unify (apply sub t) t2
                        _                        -> throwError (unifyType t1 t2)
                    Nothing  -> impossible ("Constructor with name " <> i <> " not found.")
            _                    -> throwError (unifyType t1 t2)

-- | Unifies a type alias (if there is one with that name) with a type.
unifyAlias :: Type -> Type -> SolveType (Subst Type)
unifyAlias tid t1 =
    let (TId name) = annotated tid
    in customTypeCtx `views` lookup name >>= \case
        Just cty -> case annotated cty of
            Forall _ (TypeAlias t2) -> unify t2 t1
            _                       -> throwError (unifyType tid t1)
        Nothing  -> impossible ("Type with name " <> name <> " not found.")

-- | Tries to bind a type variable to a type (checking for infinite kinds).
bind :: String -> Type -> SolveType (Subst Type)
bind a t = case annotated t of
    t' | t' == TVar a            -> do
        when ("_" `isPrefixOf` a) do
            throwError (typeHole t)
        pure mempty
       | occursCheck a t == True -> throwError (infiniteType a t)
       | otherwise               -> pure (Subst (Map.singleton a t'))
  where
    occursCheck :: String -> Type -> Bool
    occursCheck a t = a `Set.member` free t
