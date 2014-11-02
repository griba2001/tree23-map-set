{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, BangPatterns #-}
module Data.Tree23.SortedSet (
  Set,
  empty, singleton,
  null, size,        
  insert, insertAll,
  delete, deleteAll,
  member, notMember,
  fromList, toList,
  map, mapMonotonic,
  filter, partition,
  unionL, unionR, union, unions,
  difference, intersection,        
  clean,
  findMin, findMax,
) where

import Prelude hiding (null, findMin, minimum, map, filter)

import Data.Maybe
import Data.Ord
import qualified Data.List as L
import Data.Monoid (Monoid, mempty, mappend, (<>), mconcat)
import Data.Foldable (Foldable(..))
import qualified Data.Foldable as F

import qualified Data.Tree23.Tree23 as T23
import qualified Data.Tree23.Entry as E

type Set k = T23.Tree k ()

empty :: Set k
empty = T23.empty

null :: Set k -> Bool
null = T23.null

singleton :: k -> Set k
singleton x = T23.singleton x ()

-- | size O(log n)
size :: Set k -> Int
size = T23.size

-- | insert O( log n)
insert :: Ord k => k -> Set k -> Set k
insert k !set = T23.insertWith (const) (k, ()) set

insertAll :: (Ord k, Foldable t) => t k -> Set k -> Set k
insertAll xs set = F.foldl' (flip insert) set xs

-- | delete O( log n)
delete :: Ord k => k -> Set k -> Set k
delete = T23.delete

deleteAll :: (Ord k, Foldable t) => t k -> Set k -> Set k
deleteAll xs set = F.foldl' (flip delete) set xs

-- | member O( log n)
member, notMember :: Ord k => k -> Set k -> Bool
member = T23.member
notMember k = not . T23.member k

---------------------------------------------------------------

-- | fromList O( n)
fromList :: (Ord k, Foldable t) => t k -> Set k
fromList xs = insertAll xs empty

-- | toList O( n) uses DList to append the subtrees and nodes results
toList :: Set k -> [k]
toList = fst . L.unzip . T23.toList

---------------------------------------------------------------
-- | map through list conversion
map :: (Ord k1, Ord k2) => (k1 -> k2) -> Set k1 -> Set k2
map f = fromList . L.map f . toList

mapMonotonic :: (Ord k1, Ord k2) => (k1 -> k2) -> Set k1 -> Set k2
mapMonotonic f = T23.mapEntriesKeysMonotonic (E.mapEntryKey f)

----------------------------------------------------------------

filter :: (k -> Bool) -> Set k -> Set k
filter prop = T23.mapEntries (E.filterEntry prop)

partition :: (k -> Bool) -> Set k -> (Set k, Set k)
partition p xs = (filter p xs, filter (not . p) xs)

----------------------------------------------------------------

unionL, unionR, union :: Ord k => Set k -> Set k -> Set k
unionR tx ty = insertAll (toList ty) tx -- on collision it keeps last inserted
unionL tx ty = insertAll (toList tx) ty -- on collision it keeps last inserted
union = unionL

instance (Ord a) => Monoid (Set a) where  -- requires extensions TypeSynonymInstances, FlexibleInstances
  mempty = empty
  mappend = union

difference, intersection :: (Ord k) => Set k -> Set k -> Set k

-- difference O(m · log n)
difference tx ty = deleteAll (toList ty) tx

{-
-- intersection
intersection tx ty = flipValidity diff -- turn valids off, invalids on
  where
    diff = difference tx' ty
    tx' = clean tx
    flipValidity = T23.mapEntries E.flipEntryValid
    -}
    
-- intersection O(n · log m)
intersection tx ty = insertAll xs empty
  where
    xs = [x | x <- toList tx, x `member` ty]
    
----------------------------------------------------------------

unions :: (Ord k) => [Set k] -> Set k
unions [] = T23.empty
unions (hd:tl) = insertAll tailElems hd 
        where tailElems = L.concatMap toList tl
              
----------------------------------------------------------------

-- remove deleted
clean :: Ord k => Set k -> Set k
clean = fromList . toList

----------------------------------------------------------------

findMin, findMax :: Ord k => Set k -> Maybe k
findMin s = T23.minimum s >>= return . fst

findMax s = T23.maximum s >>= return . fst

