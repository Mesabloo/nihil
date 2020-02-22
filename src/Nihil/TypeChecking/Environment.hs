{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Nihil.TypeChecking.Environment where

import Nihil.TypeChecking.Core
import Nihil.TypeChecking.Substitution
import qualified Data.Map as Map
import qualified Nihil.Syntax as AC
import Control.Lens (makeLenses)

newtype Env t = Env { unwrap :: Map.Map String t }
  deriving
    ( -- | Use only for debugging
      Show
    , Monoid, Semigroup
    , Functor
    , Foldable, Traversable )

instance Substitutable t => Substitutable (Env t) where
    type Subst (Env t) = Subst t

    free = free . elems

    apply s = Env . Map.map (apply s) . unwrap

type KindEnv = Env Kind

type TypeEnv = Env (Scheme Type)

type CustomTypeEnv = Env CustomType

type InstEnv = Map.Map Type (Env AC.Expr)

-- | Removes an entry from an enviroment given its name.
remove :: Env t -> String -> Env t
remove e k = Env (Map.delete k (unwrap e))

-- | Unites two environments.
union :: Env t -> Env t -> Env t
union e1 e2 = Env (unwrap e1 `Map.union` unwrap e2)

-- | Adds an entry to the environment.
extend :: Env t -> (String, t) -> Env t
extend e (k, v) = Env (Map.insert k v (unwrap e))

-- | Looks up an entry of the environment given its name.
lookup :: String -> Env t -> Maybe t
lookup k = Map.lookup k . unwrap

-- | Fetches all names in an environment.
keys :: Env t -> [String]
keys = Map.keys . unwrap

-- | Fetches all entries from the environment.
elems :: Env t -> [t]
elems = Map.elems . unwrap

-- | Gets an entry from an environment given its name.
at :: Env t -> String -> t
at = (Map.!) . unwrap

data GlobalEnv
    = GlobalEnv
    { _typeCtx        :: KindEnv        -- ^ Type kinds holder
    , _customTypeCtx  :: CustomTypeEnv  -- ^ Data constructor holder
    , _funDefCtx      :: TypeEnv        -- ^ Function definitions holder
    , _constructorCtx :: TypeEnv        -- ^ Constructor definitions holder
    , _instanceCtx    :: InstEnv        -- ^ Instance records holder
    }
  deriving
  ( -- | Use only for debugging
    Show )
makeLenses ''GlobalEnv