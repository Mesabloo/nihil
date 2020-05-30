{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Nihil.TypeChecking.Core
( Kind(..), Type, Type'(..)
, Scheme(..)
, CustomType, CustomType'(..) ) where

import Nihil.Utils.Source
import qualified Data.Map as Map

data Kind
    = KStar                   -- ^ > { * }
    | KVar String             -- ^ > { k }
    | KApplication Kind Kind  -- ^ > { k₁ k₂ }
    | KArrow                  -- ^ > { -> } or { → }
    | KRow                    -- ^ > { Row }
  deriving
    ( -- | Use only for debugging
      Show
    , Eq )

type Type = Located Type'
data Type'
    = TId String              -- ^ > { Type }
    | TVar String             -- ^ > { a }
    | TRigid String           -- ^ > { a }
    | TTuple [Type]           -- ^ > { (a, b, c) }
    | TApplication Type Type  -- ^ > { t₁ t₂ }
    | TPrim String
    | TRow (Map.Map String Type) (Maybe Type)
                              -- ^ > { { f : t1 ; g : t2 | rest } }
    | TRecord Type            -- ^ > { ∏{ f : t1 ; g : t2 | rest } }
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
