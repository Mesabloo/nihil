-- The Great Nihil Compiler
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

{-# LANGUAGE TupleSections #-}

module Language.Nihil.TypeChecking.Rules.Kinds.Infer where

import Language.Nihil.TypeChecking.Internal.Kind
import Language.Nihil.TypeChecking.KindChecker
import Language.Nihil.TypeChecking.Internal.Type
import Language.Nihil.TypeChecking.Internal.Errors.UndefinedType
import Language.Nihil.TypeChecking.Internal.Constraint
import Language.Nihil.TypeChecking.Internal.Environment
import qualified Data.Map as Map
import Control.Lens ((^.), use, (+=), (%~))
import Control.Monad.Reader (asks, local)
import Control.Monad.Except (throwError)
import Control.Monad.Writer (tell)
import Debug.Trace (traceShow)
import Data.Maybe (fromJust)

infer :: Type -> KI Kind
infer (TId n) =
    asks (Map.lookup n . (^. _KindEnv)) >>= maybe err pure
  where err = throwError (makeUndefinedTypeError n)
infer( TVar n) =
    asks (Map.lookup (n ^. _TV) . (^. _KindEnv)) >>= maybe err pure
  where err = throwError (makeUndefinedTypeError (n ^. _TV))
infer (TRigid n) =
    asks (Map.lookup (n ^. _TV) . (^. _KindEnv)) >>= maybe err pure
  where err = throwError (makeUndefinedTypeError (n ^. _TV))
infer (TTuple ts) = do
    mapM infer ts
    pure KType
infer (TFun (t1, _) t2) = do
    k1 <- infer t1
    k2 <- infer t2
    tell [k1 :*~: KType, k2 :*~: KType]
    pure KType
infer (TApp f t) = do
    k1 <- infer f
    k2 <- infer t
    kv <- fresh "k"
    tell [k1 :*~: (k2 `KArr` kv)]
    pure kv
infer t = traceShow t undefined

fresh :: String -> KI Kind
fresh k = do
    s <- use kiSupply
    kiSupply += 1

    env <- Map.keys <$> asks (^. _KindEnv)
    let newKVar = k <> show s

    if newKVar `notElem` env
    then pure (KVar (KV newKVar))
    else fresh newKVar

-- | Kind checking for custom types.
kiCustomScheme :: CustomScheme -> KI Kind
kiCustomScheme (CustomScheme tvs t) = do
    typeArgs <- Map.fromList <$> mapM ((<$> fresh "k") . (,)) tvs
    k' <- local (_KindEnv %~ Map.union typeArgs) $ case t of
        -- If it is a sum type, kind checking all of its constructors
        TSum constrs -> KType <$ kiConstrs (Map.toList constrs)
        -- If it is a type alias, kind check the type
        TAlias t -> infer t
        _ -> undefined

    let k = foldr KArr k' (fromJust . flip Map.lookup typeArgs <$> tvs)
    pure k
  where foldConstr (TFun (t1, _) t2) = t1 : foldConstr t2
        foldConstr t = []

        kiConstrs [] = pure ()
        kiConstrs ((n, Scheme _ c):cs) = do
            kiConstr n (foldConstr c)
            kiConstrs cs

        kiConstr _ [] = pure ()
        kiConstr n (t:ts) = do
            k <- infer t
            tell [k :*~: KType]
            kiConstr n ts


-- | Transforms a 'Scheme' into a pair composed of a 'KindSubst' and a 'Kind'.
kiScheme :: Scheme -> KI (Kind)
kiScheme (Scheme vars t) = do
    nvars <- mapM (const $ fresh "k") vars
    let s = Map.fromList (zipWith (\(TV v) n -> (v, n)) vars nvars)
    local (_KindEnv %~ Map.union s) (infer t)