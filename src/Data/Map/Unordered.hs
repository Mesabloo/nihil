{-# LANGUAGE NoImplicitPrelude #-}

-- | An unordered, strict map.

module Data.Map.Unordered
    ( Map, empty, lookup, insert, delete, fromList, toList, map, singleton, insertWith
    , member, elems, unionWith, difference, union, size, null
    , intersection, foldrWithKey, foldlWithKey', keys
    , adjust, update, alter
    , unions, toHashMap, fromHashMap, filter, filterWithKey, keys, lookupDefault
    , mapMaybe, mapWithKey
    )
where


import Data.HashMap.Strict.InsOrd


type Map = InsOrdHashMap