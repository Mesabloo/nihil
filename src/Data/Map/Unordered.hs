{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | An unordered, strict map.
module Data.Map.Unordered
    ( USMap, Map, empty, lookup, insert, delete, fromList, toList, map, singleton, insertWith
    , member, elems, unionWith, difference, union, findWithDefault, size, null, isSubmapOf
    , intersection, foldrWithKey, foldlWithKey, foldlWithKey', keys, insertLookupWithKey
    , updateLookupWithKey, adjust, deleteLookup, assocs, insertWith', update, alter
    , lookup', unions, toHashMap, fromHashMap, filter, filterWithKey, keysSet, lookupDefault
    , fromListWith, mapMaybe, unionsWith

-----------------------------------------------------------------------------------------------------

    , mapWithKey
    )
where


import Control.DeepSeq (NFData(..))
import Data.Data
import Data.Hashable (Hashable(..))
import Data.Maybe (isJust, fromMaybe)
import Prelude hiding (map, lookup, null, filter, pred)
import Test.QuickCheck (Arbitrary(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as Set
import qualified Data.List as List


type Map = USMap

newtype USMap k v = USMap { unUSMap :: HM.HashMap k v }
    deriving (Eq, Functor, Foldable, Traversable, Typeable, Data, Monoid, Semigroup)

instance (Hashable k, Eq k, Read k, Read v) => Read (USMap k v) where
    readsPrec p s =
        do (l, r) <- readsPrec p s
           return (fromList l, r)

instance (Show k, Show v) => Show (USMap k v) where
    showsPrec p usmap = showsPrec p (toList usmap)

instance (Hashable k, Hashable v) => Hashable (USMap k v) where
    hashWithSalt s (USMap hm) = hashWithSalt s hm

instance (NFData k, NFData v) => NFData (USMap k v) where
    rnf (USMap x) = rnf x

instance (Hashable k, Eq k, Arbitrary k, Arbitrary v) => Arbitrary (USMap k v) where
    arbitrary = fromList <$> arbitrary

toHashMap :: USMap k v -> HM.HashMap k v
toHashMap = unUSMap

fromHashMap :: HM.HashMap k v -> USMap k v
fromHashMap = USMap

empty :: USMap k v
empty = USMap HM.empty

member :: (Eq k, Hashable k) => k -> USMap k v -> Bool
member k (USMap m) =
    case HM.lookup k m of
      Just _ -> True
      Nothing -> False

lookupDefault :: (Eq k, Hashable k) => v -> k -> USMap k v -> v
lookupDefault d k (USMap hm) = HM.lookupDefault d k hm

{-# SPECIALISE lookup :: (Eq k, Hashable k, Show k) => k -> USMap k v -> Maybe v #-}
{-# INLINEABLE lookup #-}
lookup :: (Eq k, Show k, Hashable k, Monad m) => k -> USMap k v -> m v
lookup k (USMap hm) =
    case HM.lookup k hm of
      Nothing -> fail ("Key " ++ show k ++ " not found.")
      Just x -> return x

{-# INLINE lookup' #-}
lookup' :: (Eq k, Hashable k) => k -> USMap k v -> Maybe v
lookup' k (USMap hm) = HM.lookup k hm

insert :: (Eq k, Hashable k) => k -> v -> USMap k v -> USMap k v
insert k v (USMap hm) = USMap (HM.insert k v hm)

delete :: (Eq k, Hashable k) => k -> USMap k v -> USMap k v
delete k (USMap hm) = USMap (HM.delete k hm)

fromList :: (Eq k, Hashable k) => [(k,v)] -> USMap k v
fromList = USMap . HM.fromList

fromListWith :: (Eq k, Hashable k) => (v -> v -> v) -> [(k, v)] -> USMap k v
fromListWith f = USMap . HM.fromListWith f

toList :: USMap k v -> [(k, v)]
toList (USMap hm) = HM.toList hm

map :: (v -> v') -> USMap k v -> USMap k v'
map f = USMap . HM.map f . unUSMap

mapMaybe :: (v -> Maybe v') -> USMap k v -> USMap k v'
mapMaybe f = USMap . HM.mapMaybe f . unUSMap

singleton :: Hashable k => k -> v -> USMap k v
singleton k = USMap . HM.singleton k

insertWith :: (Eq k, Hashable k) => (v -> v -> v) -> k -> v -> USMap k v -> USMap k v
insertWith f k v (USMap hm) = USMap (HM.insertWith f k v hm)

elems :: USMap k v -> [v]
elems = HM.elems . unUSMap

keys :: USMap k v -> [k]
keys = HM.keys . unUSMap

keysSet :: (Eq k, Hashable k) => USMap k v -> Set.HashSet k
keysSet = Set.fromList . fmap fst . toList

union :: (Hashable k, Eq k) => USMap k v -> USMap k v -> USMap k v
union (USMap m1) (USMap m2) = USMap (HM.union m1 m2)

unionWith :: (Hashable k, Eq k) => (v -> v -> v) -> USMap k v -> USMap k v -> USMap k v
unionWith f (USMap m1) (USMap m2) = USMap (HM.unionWith f m1 m2)

difference :: (Eq k, Hashable k) => USMap k v -> USMap k w -> USMap k v
difference (USMap m1) (USMap m2) = USMap (HM.difference m1 m2)

intersection :: (Eq k, Hashable k) => USMap k v -> USMap k w -> USMap k v
intersection (USMap m1) (USMap m2) = USMap (HM.intersection m1 m2)

findWithDefault :: (Eq k, Hashable k) => a -> k -> USMap k a -> a
findWithDefault def k (USMap m) = fromMaybe def (HM.lookup k m)

size :: USMap k v -> Int
size = HM.size . unUSMap

null :: USMap k v -> Bool
null = HM.null . unUSMap

isSubmapOf :: (Hashable k, Eq k) => USMap k a -> USMap k a -> Bool
isSubmapOf a b = null (a `difference` b)

foldrWithKey :: (k -> v -> a -> a) -> a -> USMap k v -> a
foldrWithKey f a (USMap hm) = HM.foldrWithKey f a hm

{-# WARNING foldlWithKey "This function is strict.  Better explicitly use USMap.foldlWithKey'" #-}
foldlWithKey :: (a -> k -> v -> a) -> a -> USMap k v -> a
foldlWithKey f a (USMap hm) = HM.foldlWithKey' f a hm

foldlWithKey' :: (a -> k -> v -> a) -> a -> USMap k v -> a
foldlWithKey' f a (USMap hm) = HM.foldlWithKey' f a hm

filterWithKey :: (k -> v -> Bool) -> USMap k v -> USMap k v
filterWithKey pred (USMap hm) = USMap $! HM.filterWithKey pred hm

filter :: (v -> Bool) -> USMap k v -> USMap k v
filter pred (USMap hm) = USMap $! HM.filter pred hm

insertLookupWithKey :: (Eq k, Hashable k) => (k -> a -> a -> a) -> k -> a -> USMap k a
                    -> (Maybe a, USMap k a)
insertLookupWithKey f k newV m =
    case lookup' k m of
      justV@(Just oldV) -> (justV, insert k (f k newV oldV) m)
      nothing@Nothing -> (nothing, insert k newV m)

updateLookupWithKey :: (Eq k, Hashable k) => (k -> a -> Maybe a) -> k -> USMap k a
                    -> (Maybe a, USMap k a)
updateLookupWithKey f k m =
    case lookup' k m of
      Just oldV ->
          case f k oldV of
            justV@(Just newV) -> (justV, insert k newV m)
            Nothing -> (Just oldV, delete k m)
      Nothing -> (Nothing, m)

adjust :: (Eq k, Hashable k) => (a -> a) -> k -> USMap k a -> USMap k a
adjust f k (USMap hm) = USMap (HM.adjust f k hm)

deleteLookup :: (Eq k, Hashable k) => k -> USMap k a -> (Maybe a, USMap k a)
deleteLookup k m = (lookup' k m, delete k m)

{-# INLINE assocs #-}
assocs :: USMap k a -> [(k, a)]
assocs = HM.toList . unUSMap

{-# INLINE insertWith' #-}
insertWith' :: (Eq k, Hashable k) => (a -> a -> a) -> k -> a -> USMap k a -> USMap k a
insertWith' f k !v (USMap hm) = USMap (HM.insertWith f k v hm)

{-# INLINE update #-}
update :: (Eq k, Hashable k) => (a -> Maybe a) -> k -> USMap k a -> USMap k a
update f k um@(USMap hm) =
    case HM.lookup k hm of
      Just curV ->
          case f curV of
            Nothing -> USMap (HM.delete k hm)
            Just newV -> USMap (HM.insert k newV hm)
      Nothing -> um

{-# INLINE alter #-}
alter :: (Eq k, Hashable k) => (Maybe a -> Maybe a) -> k -> USMap k a -> USMap k a
alter f k um@(USMap hm) =
    let mOld = HM.lookup k hm
        mNew = f mOld
    in case mNew of
         Nothing
             | isJust mOld -> USMap (HM.delete k hm)
             | otherwise -> um
         Just new -> USMap (HM.insert k new hm)

unions :: (Hashable k, Eq k) => [USMap k a] -> USMap k a
unions = List.foldl' union empty

unionsWith :: (Hashable k, Eq k) => (a -> a -> a) -> [USMap k a] -> USMap k a
unionsWith f = List.foldl' (unionWith f) empty

----------------------------------------------------------------------------------------------------------

mapWithKey :: (Hashable k, Eq k) => (k -> a -> b) -> USMap k a -> USMap k b
mapWithKey f = USMap . HM.mapWithKey f . unUSMap