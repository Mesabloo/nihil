{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | An unordered, strict map.
--
-- Based on <https://hackage.haskell.org/package/insert-ordered-containers>.

module Data.Map.Unordered
( Map
, module Data.HashMap.Strict.InsOrd ) where

import Data.HashMap.Strict.InsOrd
import Data.Align (Semialign(..), Unalign(..))
import Data.These(These(..))
import Data.Hashable (Hashable)
import Prelude (Eq, mempty, error)
import Data.Bifunctor (first, second, bimap)

-- | Convenient name
type Map = InsOrdHashMap

instance (Hashable k, Eq k) => Semialign (Map k) where
    align m n = unionWith merge (map This m) (map That n)
      where merge (This a) (That b) = These a b
            merge _ _ = error "Align (UMap.Map k): internal error"
    --zipWith = intersectionWith

instance (Hashable k, Eq k) => Unalign (Map k) where
    unalign = foldrWithKey f (mempty, mempty)
      where f k (This a)    = first (insert k a)
            f k (That a)    = second (insert k a)
            f k (These a b) = bimap (insert k a) (insert k b)
