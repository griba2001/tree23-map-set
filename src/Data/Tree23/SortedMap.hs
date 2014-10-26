{-# LANGUAGE PackageImports, BangPatterns #-}
module Data.Tree23.SortedMap (
  Map,
  size,
  empty,
  singleton,
  insert, insertWith, insertAll,
  delete, deleteAll,
  member, notMember,
  lookup,        
  fromList,
  toList,
  keys, values,
  map,
  mapKeys,        
  mapKeysMonotonic,
  filter, partition,
  unionWith, unionL, unionR, union, 
  unions, unionsWith,
  clean,
  findMin, findMax,
) where

import Prelude hiding (map, filter, lookup)

import Data.Maybe
import Data.Ord
import qualified Data.List as L
import qualified "dlist" Data.DList as D
import Data.Monoid (Monoid, mempty, mappend, (<>), mconcat)
import Data.Foldable (Foldable(..))
import qualified Data.Foldable as F

import qualified Data.Tree23.Tree23 as T23
-- import Data.Tree23.MinMax
import qualified Data.Tree23.Entry as E

-- import Data.Tree23.Pair

type Map k v = T23.Tree k v

empty :: Map k v
empty = T23.empty

singleton :: k -> v -> Map k v
singleton = T23.singleton

size :: Map k v -> Int
size = T23.size

-- insert or update (strict to avoid O(n) stack pending ops when used with List.foldl').
insert :: Ord k => (k, v) -> Map k v -> Map k v
insert = T23.insertWith (const)

insertWith :: Ord k => (v -> v -> v) -> (k, v) -> Map k v -> Map k v
insertWith = T23.insertWith

insertAll :: (Ord k, Foldable t) => t (k, v) -> Map k v -> Map k v
insertAll xs map = F.foldl' (flip insert) map xs

delete :: Ord k => k -> Map k v -> Map k v
delete = T23.deleteB

deleteAll :: (Ord k, Foldable t) => t k -> Map k v -> Map k v
deleteAll xs map = F.foldl' (flip delete) map xs

member, notMember :: Ord k => k -> Map k v -> Bool
member = T23.containsB
notMember k = not . T23.containsB k

lookup :: Ord k => k -> Map k v -> Maybe (k, v)
lookup = T23.lookupB
---------------------------------------------------------------

fromList :: (Ord k, Foldable t) => t (k, v) -> Map k v
fromList xs = F.foldl' (flip insert) empty xs

toList :: Map k v -> [(k, v)]
toList = D.toList . T23.toDList

---------------------------------------------------------------

keys :: Map k v -> [k]
keys = fst . L.unzip . toList

values  :: Map k v -> [v]
values = snd . L.unzip . toList

---------------------------------------------------------------

map :: (v1 -> v2) -> Map k v1 -> Map k v2
map f = T23.mapEntriesValues (E.mapEntryValue f)

mapKeys :: (Ord k1, Ord k2) => (k1 -> k2) -> Map k1 v -> Map k2 v
mapKeys f = fromList . L.map (\(k, v) -> (f k, v)) . toList

mapKeysMonotonic :: (Ord k1, Ord k2) => (k1 -> k2) -> Map k1 v -> Map k2 v
mapKeysMonotonic f = T23.mapEntriesKeysMonotonic (E.mapEntryKey f)

----------------------------------------------------------------

filter :: (k -> Bool) -> Map k v -> Map k v
filter prop = T23.mapEntriesMonotonic (E.filterEntry prop)

partition :: (k -> Bool) -> Map k v -> (Map k v, Map k v)
partition p xs = (filter p xs, filter (not . p) xs)

----------------------------------------------------------------

unionWith :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWith f tx ty = L.foldl' (flip (T23.insertWith f)) ty (toList tx)

unionL, unionR, union :: Ord k => Map k v -> Map k v -> Map k v
unionR tx ty = L.foldl' (flip insert) tx (toList ty) -- on collision it keeps last inserted
unionL tx ty = L.foldl' (flip insert) ty (toList tx) -- on collision it keeps last inserted
union = unionL

----------------------------------------------------------------

unions :: (Ord k) => [Map k v] -> Map k v
unions [] = T23.empty
unions (hd:tl) = L.foldl' (flip insert) hd tailElems
        where -- tailElems :: [a]
              tailElems = L.concatMap toList tl

unionsWith :: (Ord k) => (a -> a -> a) -> [Map k a] -> Map k a
unionsWith f [] = T23.empty
unionsWith f (hd:tl) = L.foldl' (flip (T23.insertWith f)) hd tailElems
        where tailElems = L.concatMap toList tl
----------------------------------------------------------------

-- remove deleted
clean :: Ord k => Map k v -> Map k v
clean = fromList . toList

----------------------------------------------------------------

findMin :: Ord k => Map k v -> Maybe (k, v)
findMin = T23.minimum E.entryItem

findMax :: Ord k => Map k v -> Maybe (k, v)
findMax = T23.maximum E.entryItem
