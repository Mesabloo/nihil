-- Based on <https://hackage.haskell.org/package/insert-ordered-containers>.
-- Copyright (c) 2019 Mesabloo

-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--   * Redistributions of source code must retain the above copyright
--     notice, this list of conditions and the following disclaimer.
--   * Redistributions in binary form must reproduce the above copyright
--     notice, this list of conditions and the following disclaimer in
--     the documentation and/or other materials provided with the
--     distribution.
--   * Neither the name of Mesabloo nor the names of its
--     contributors may be used to endorse or promote products derived
--     from this software without specific prior written permission.


{-# LANGUAGE NoImplicitPrelude, TypeSynonymInstances, TypeFamilies #-}

-- | An unordered, strict map.

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

instance (Hashable k, Eq k) => Unalign (Map k) where
    unalign = foldrWithKey f (mempty, mempty)
      where f k (This a)    = first (insert k a)
            f k (That a)    = second (insert k a)
            f k (These a b) = bimap (insert k a) (insert k b)