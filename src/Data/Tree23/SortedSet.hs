{-# LANGUAGE PackageImports, BangPatterns #-}
module Data.Tree23.SortedSet (
  Set,
  size,
  empty,
  singleton,
  insert, insertAll,
  delete, deleteAll,
  member, notMember,
  fromList,
  toList,
  map,
  mapMonotonic,
  filter, partition,
  unionL, unionR, union,
  unions,
  clean,
  findMin, findMax,
) where

import Prelude hiding (findMin, minimum, map, filter)

import Data.Maybe
import Data.Ord
import qualified Data.List as L
import qualified "dlist" Data.DList as D
import Data.Monoid (Monoid, mempty, mappend, (<>), mconcat)
import Data.Foldable (Foldable(..))
import qualified Data.Foldable as F

import qualified Data.Tree23.Tree23 as T23
import qualified Data.Tree23.Entry as E

type Set k = T23.Tree k ()

empty :: Set k
empty = T23.empty

singleton :: k ->Set k
singleton x = T23.singleton x ()

-- | size O(log n)
size :: Set k -> Int
size = T23.size

-- | insert O( log n)
insert :: Ord k => k -> Set k -> Set k
insert k = T23.insertWith (const) (k, ())

insertAll :: (Ord k, Foldable t) => t k -> Set k -> Set k
insertAll xs set = F.foldl' (flip insert) set xs

-- | delete O( log n)
delete :: Ord k => k -> Set k -> Set k
delete k = T23.deleteB k

deleteAll :: (Ord k, Foldable t) => t k -> Set k -> Set k
deleteAll xs set = F.foldl' (flip delete) set xs

-- | member O( log n)
member, notMember :: Ord k => k -> Set k -> Bool
member k = T23.containsB k
notMember k = not . T23.containsB k

---------------------------------------------------------------

-- | fromList O( n)
fromList :: (Ord k, Foldable t) => t k -> Set k
fromList xs = F.foldl' (flip insert) empty xs

-- | toList O( n) uses DList to append the subtrees and nodes results
toList :: Set k -> [k]
toList = fst . L.unzip . D.toList . T23.toDList

---------------------------------------------------------------
-- | map through list conversion
map :: (Ord k1, Ord k2) => (k1 -> k2) -> Set k1 -> Set k2
map f = fromList . L.map f . toList

mapMonotonic :: (Ord k1, Ord k2) => (k1 -> k2) -> Set k1 -> Set k2
mapMonotonic f = T23.mapEntriesKeysMonotonic (E.mapEntryKey f)

----------------------------------------------------------------

filter :: (k -> Bool) -> Set k -> Set k
filter prop = T23.mapEntriesMonotonic (E.filterEntry prop)

partition :: (k -> Bool) -> Set k -> (Set k, Set k)
partition p xs = (filter p xs, filter (not . p) xs)

----------------------------------------------------------------

unionL, unionR, union :: Ord k => Set k -> Set k -> Set k
unionR tx ty = L.foldl' (flip insert) tx (toList ty) -- on collision it keeps last inserted
unionL tx ty = L.foldl' (flip insert) ty (toList tx) -- on collision it keeps last inserted
union = unionL

----------------------------------------------------------------

unions :: (Ord k) => [Set k] -> Set k
unions [] = T23.empty
unions (hd:tl) = L.foldl' (flip insert) hd tailElems
        where tailElems = L.concatMap toList tl
              
----------------------------------------------------------------

-- remove deleted
clean :: Ord k => Set k -> Set k
clean = fromList . toList

----------------------------------------------------------------

findMin :: Ord k => Set k -> Maybe k
findMin = T23.minimum E.key

findMax :: Ord k => Set k -> Maybe k
findMax = T23.maximum E.key

