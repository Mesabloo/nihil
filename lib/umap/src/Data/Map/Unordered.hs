{-# LANGUAGE NoImplicitPrelude #-}

-- | An unordered, strict map.

module Data.Map.Unordered
( Map
, module Data.HashMap.Strict.InsOrd ) where

import Data.HashMap.Strict.InsOrd

type Map = InsOrdHashMap