{-# LANGUAGE NoImplicitPrelude, TypeSynonymInstances, TypeFamilies #-}

-- | An unordered, strict map.

module Data.Map.Unordered
( Map
, module Data.HashMap.Strict.InsOrd ) where

import Data.HashMap.Strict.InsOrd
import qualified Data.Key as K (Key, Keyed(..))
import Data.Align.Key (AlignWithKey)
import Data.Align (Align(..))
import Data.Hashable (Hashable(..))
import Data.These(These(..))
import Prelude (Eq, mempty, error)

type Map = InsOrdHashMap

instance (Hashable k, Eq k) => AlignWithKey (Map k)

type instance K.Key (Map k) = k

instance (Hashable k, Eq k) => K.Keyed (Map k) where
    mapWithKey = mapWithKey

instance (Hashable k, Eq k) => Align (Map k) where
    nil = mempty
    align m n = unionWith merge (map This m) (map That n)
      where merge (This a) (That b) = These a b
            merge _ _ = error "Align (UMap.Map k): internal error"