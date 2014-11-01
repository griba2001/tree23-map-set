{-# LANGUAGE PackageImports, BangPatterns #-}
module Data.Tree23.Tree23 (
  Tree,
  empty, singleton,
  null, size,
  insert, insertWith,
  delete, update,
  member, lookup,
  mapEntries, mapEntriesValues, mapEntriesKeysMonotonic,        
  minimum, maximum,
  toList,
) where

import Prelude hiding (null, lookup, maximum, minimum)
import Data.Maybe as M
import Data.Ord
import qualified "dlist" Data.DList as D
import qualified Safe
import qualified Data.List as L

import Data.Tree23.Entry as E

data Tree k v = Nil | Branch2 (Tree k v) (Entry k v) (Tree k v)
               | Branch3 (Tree k v) (Entry k v) (Tree k v) (Entry k v) (Tree k v) deriving (Eq, Show)

data Result k v = ResTree (Tree k v) | ResBranch4 (Tree k v) (Entry k v) (Tree k v) (Entry k v) (Tree k v) (Entry k v) (Tree k v)

empty :: Tree k v
empty = Nil

singleton :: k -> v -> Tree k v
singleton k v = Branch2 Nil (Entry k v Valid) Nil

null :: Tree k v -> Bool
null Nil = True
null _ = False

size :: Tree k v -> Int
size Nil = 0
size (Branch2 esq _ dreta) = 1 + size esq + size dreta
size (Branch3 esq _ mig _ dreta) = 2 + size esq + size mig + size dreta



-- insert or update (strict to avoid O(n) stack pending ops when used with List.foldl').
insertWith :: Ord k => (v -> v -> v) -> (k, v) -> Tree k v -> Tree k v
insertWith f (k, v) Nil = singleton k v
insertWith f (k, v) !arb = case insert f (Entry k v Valid) arb of
                     ResTree res -> res
                     ResBranch4 f1 a f2 b f3 c f4 -> Branch2 (Branch2 f1 a f2) b (Branch2 f3 c f4)


-- insert entry with collision combine function f
insert :: Ord k => (v -> v -> v) -> Entry k v -> Tree k v -> Result k v

insert f x Nil = ResTree $ Branch2 Nil x Nil

insert f x (Branch2 Nil y Nil)
    | x == y = ResTree $ Branch2 Nil (combineEntry f y x) Nil
    | x < y = ResTree $ Branch3 Nil x Nil y Nil
    | otherwise = ResTree $ Branch3 Nil y Nil x Nil

insert f x (Branch3 Nil y Nil z Nil)
    | x == y = ResTree $ Branch3 Nil (combineEntry f y x) Nil z Nil
    | x == z = ResTree $ Branch3 Nil y Nil (combineEntry f z x) Nil
    | x < y = ResBranch4 Nil x Nil y Nil z Nil
    | x < z = ResBranch4 Nil y Nil x Nil z Nil
    | otherwise = ResBranch4 Nil y Nil z Nil x Nil

insert f x (Branch2 esq y dreta)
        | x == y = ResTree $ Branch2 esq (combineEntry f y x) dreta
        | x < y = case insert f x esq of
                       ResTree arb -> ResTree $ Branch2 arb y dreta
                       ResBranch4 f1 a f2 b f3 c f4 -> ResTree $ Branch3 (Branch2 f1 a f2) b (Branch2 f3 c f4) y dreta
        | otherwise = case insert f x dreta of
                           ResTree arb -> ResTree $ Branch2 esq y arb
                           ResBranch4 f1 a f2 b f3 c f4 -> ResTree $ Branch3 esq y (Branch2 f1 a f2) b (Branch2 f3 c f4)

insert f x (Branch3 esq y mig z dreta)
        | x == y = ResTree $ Branch3 esq (combineEntry f y x) mig z dreta
        | x == z = ResTree $ Branch3 esq y mig (combineEntry f z x) dreta
        | x < y = case insert f x esq of
                        ResTree arb -> ResTree $ Branch3 arb y mig z dreta
                        ResBranch4 f1 a f2 b f3 c f4 -> ResBranch4 (Branch2 f1 a f2) b (Branch2 f3 c f4) y mig z dreta
        | x < z = case insert f x mig of
                        ResTree arb -> ResTree $ Branch3 esq y arb z dreta
                        ResBranch4 f1 a f2 b f3 c f4 -> ResBranch4 esq y (Branch2 f1 a f2) b (Branch2 f3 c f4) z dreta
        | otherwise = case insert f x dreta of
                          ResTree arb -> ResTree $ Branch3 esq y mig z arb
                          ResBranch4 f1 a f2 b f3 c f4 -> ResBranch4 esq y mig z (Branch2 f1 a f2) b (Branch2 f3 c f4)


update :: Ord k => Entry k v -> Tree k v -> Tree k v
update _ Nil = Nil

update x ar @ (Branch2 Nil y Nil)
        | x == y = Branch2 Nil x Nil
        | otherwise = ar

update x ar @ (Branch3 Nil y Nil z Nil)
        | x == y = Branch2 Nil z Nil
        | x == z = Branch2 Nil y Nil
        | otherwise = ar

update x ar @ (Branch2 esq y dreta)
        | x == y = Branch2 esq x dreta
        | x < y = Branch2 (update x esq) y dreta
        | otherwise = Branch2 esq y (update x dreta)

update x ar @ (Branch3 esq y mig z dreta)
        | x == y = Branch3 esq x mig z dreta
        | x == z = Branch3 esq y mig x dreta
        | x < y = Branch3 (update x esq) y mig z dreta
        | x < z = Branch3 esq y (update x mig) z dreta
        | otherwise = Branch3 esq y mig z (update x dreta)

lookup :: Ord k => k -> Tree k v -> Maybe (k, v)
lookup k Nil = Nothing
lookup k (Branch2 esq y dreta)
        | k == key y = E.toMaybe y
        | k < key y = lookup k esq
        | otherwise = lookup k dreta

lookup k (Branch3 esq y mig z dreta)
        | k == key y = E.toMaybe y
        | k == key z = E.toMaybe z
        | k < key y = lookup k esq
        | k < key z = lookup k mig
        | otherwise = lookup k dreta

member :: Ord k => k -> Tree k v -> Bool
member k t = isJust $ lookup k t

delete :: Ord k => k -> Tree k v -> Tree k v
delete _ Nil = Nil

delete k ar @ (Branch2 esq y dreta)
        | k == key y = Branch2 esq (invalidate y) dreta
        | k < key y = Branch2 (delete k esq) y dreta
        | otherwise = Branch2 esq y (delete k dreta)

delete k ar @ (Branch3 esq y mig z dreta)
        | k == key y = Branch3 esq (invalidate y) mig z dreta
        | k == key z = Branch3 esq y mig (invalidate z) dreta
        | k < key y = Branch3 (delete k esq) y mig z dreta
        | k < key z = Branch3 esq y (delete k mig) z dreta
        | otherwise = Branch3 esq y mig z (delete k dreta)

toDList :: Tree k v -> D.DList (k, v)
toDList Nil = D.empty
toDList (Branch2 esq x dreta) = toDList esq `D.append` valToDList x `D.append` toDList dreta
toDList (Branch3 esq x mig y dreta) = toDList esq `D.append` valToDList x `D.append` toDList mig `D.append` valToDList y `D.append` toDList dreta

toList :: Tree k v -> [(k, v)]
toList = D.toList . toDList

mapEntriesValues :: (Entry k v1 -> Entry k v2) -> Tree k v1 -> Tree k v2
mapEntriesValues f Nil = Nil
mapEntriesValues f (Branch2 esq x dreta) = Branch2 (mapEntriesValues f esq) (f x) (mapEntriesValues f dreta)
mapEntriesValues f (Branch3 esq x mig y dreta) = Branch3 (mapEntriesValues f esq) (f x) (mapEntriesValues f mig) (f y) (mapEntriesValues f dreta)

mapEntriesKeysMonotonic :: (Ord k1, Ord k2) => (Entry k1 v -> Entry k2 v) -> Tree k1 v -> Tree k2 v
mapEntriesKeysMonotonic f Nil = Nil
mapEntriesKeysMonotonic f (Branch2 esq x dreta) = Branch2 (mapEntriesKeysMonotonic f esq) (f x) (mapEntriesKeysMonotonic f dreta)
mapEntriesKeysMonotonic f (Branch3 esq x mig y dreta) = Branch3 (mapEntriesKeysMonotonic f esq) (f x) (mapEntriesKeysMonotonic f mig) (f y) (mapEntriesKeysMonotonic f dreta)

mapEntries :: (Entry k v -> Entry k v) -> Tree k v -> Tree k v
mapEntries f Nil = Nil
mapEntries f (Branch2 esq x dreta) = Branch2 (mapEntries f esq) (f x) (mapEntries f dreta)
mapEntries f (Branch3 esq x mig y dreta) = Branch3 (mapEntries f esq) (f x) (mapEntries f mig) (f y) (mapEntries f dreta)

maximum :: Ord k => (Entry k v -> b) -> Tree k v -> Maybe b
maximum _ Nil = Nothing
maximum f (Branch2 esq x dreta)
        | isValid x = firstOfMaybes [(maximum f dreta), Just $ f x]
        | otherwise = firstOfMaybes [(maximum f dreta), (maximum f esq)]

maximum f (Branch3 esq x mig y dreta)
        | isValid y = firstOfMaybes [(maximum f dreta), Just $ f y]
        | isValid x = firstOfMaybes [(maximum f dreta), (maximum f mig), Just $ f x]
        | otherwise = firstOfMaybes [(maximum f dreta), (maximum f mig), (maximum f esq)]

minimum :: Ord k =>(Entry k v -> b) -> Tree k v -> Maybe b
minimum _ Nil = Nothing
minimum f (Branch2 esq x dreta)
        | isValid x = firstOfMaybes [(minimum f esq), Just $ f x]
        | otherwise = firstOfMaybes [(minimum f esq), (minimum f dreta)]

minimum f (Branch3 esq x mig y dreta)
        | isValid x = firstOfMaybes [(minimum f esq), Just $ f x]
        | isValid y = firstOfMaybes [(minimum f esq), (minimum f mig), Just $ f y]
        | otherwise = firstOfMaybes [(minimum f esq), (minimum f mig), (minimum f dreta)]

-- private

firstOfMaybes :: [Maybe a] -> Maybe a
firstOfMaybes xs = Safe.headDef Nothing (L.dropWhile isNothing xs)

------------------------------------------------------------------------------------
{-
foldMapKey :: Monoid m => (k -> m) -> Tree k v -> m
foldMapKey f Nil = mempty
  -- foldMap elements from the right
foldMapKey f (Branch2 l e1 r) = foldMapKey f r <> foldEntryKey f e1 <> foldMapKey f l
foldMapKey f (Branch3 l e1 mid e2 r) = foldMapKey f r <> foldEntryKey f e2 <> foldMapKey f mid <> foldEntryKey f e1 <> foldMapKey f l


foldMapVal :: Monoid m => (v -> m) -> Tree k v -> m
foldMapVal f Nil = mempty
  -- foldMap elements from the right
foldMapVal f (Branch2 l e1 r) = foldMapVal f r <> foldEntryVal f e1 <> foldMapVal f l
foldMapVal f (Branch3 l e1 mid e2 r) = foldMapVal f r <> foldEntryVal f e2 <> foldMapVal f mid <> foldEntryVal f e1 <> foldMapVal f l
-}