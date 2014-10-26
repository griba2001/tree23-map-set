{-# LANGUAGE PackageImports, BangPatterns #-}
module Data.Tree23.Tree23 where

import Prelude hiding (maximum, minimum)
import Data.Maybe as M
import Data.Ord
import qualified "dlist" Data.DList as D
import Safe
import qualified Data.List as L

import Data.Tree23.Entry as E

data Tree k v = Nil | Branch2 (Tree k v) (Entry k v) (Tree k v)
               | Branch3 (Tree k v) (Entry k v) (Tree k v) (Entry k v) (Tree k v) deriving (Eq, Show)

data Result k v = ResT (Tree k v) | Branch4 (Tree k v) (Entry k v) (Tree k v) (Entry k v) (Tree k v) (Entry k v) (Tree k v)

empty :: Tree k v
empty = Nil

singleton :: k -> v -> Tree k v
singleton k v = Branch2 Nil (Entry k v Valid) Nil

size :: Tree k v -> Int
size Nil = 0
size (Branch2 esq _ dreta) = 1 + size esq + size dreta
size (Branch3 esq _ mig _ dreta) = 2 + size esq + size mig + size dreta



-- insert or update (strict to avoid O(n) stack pending ops when used with List.foldl').
insertWith :: Ord k => (v -> v -> v) -> (k, v) -> Tree k v -> Tree k v
insertWith f (k, v) Nil = singleton k v
insertWith f (k, v) !arb = case insertB f (Entry k v Valid) arb of
                     ResT res -> res
                     Branch4 f1 a f2 b f3 c f4 -> Branch2 (Branch2 f1 a f2) b (Branch2 f3 c f4)


-- insert entry with collision combine function f
insertB :: Ord k => (v -> v -> v) -> Entry k v -> Tree k v -> Result k v

insertB f x Nil = ResT $ Branch2 Nil x Nil

insertB f x (Branch2 Nil y Nil)
    | x == y = ResT $ Branch2 Nil (combineEntry f y x) Nil
    | x < y = ResT $ Branch3 Nil x Nil y Nil
    | otherwise = ResT $ Branch3 Nil y Nil x Nil

insertB f x (Branch3 Nil y Nil z Nil)
    | x == y = ResT $ Branch3 Nil (combineEntry f y x) Nil z Nil
    | x == z = ResT $ Branch3 Nil y Nil (combineEntry f z x) Nil
    | x < y = Branch4 Nil x Nil y Nil z Nil
    | x < z = Branch4 Nil y Nil x Nil z Nil
    | otherwise = Branch4 Nil y Nil z Nil x Nil

insertB f x (Branch2 esq y dreta)
        | x == y = ResT $ Branch2 esq (combineEntry f y x) dreta
        | x < y = case insertB f x esq of
                       ResT arb -> ResT $ Branch2 arb y dreta
                       Branch4 f1 a f2 b f3 c f4 -> ResT $ Branch3 (Branch2 f1 a f2) b (Branch2 f3 c f4) y dreta
        | otherwise = case insertB f x dreta of
                           ResT arb -> ResT $ Branch2 esq y arb
                           Branch4 f1 a f2 b f3 c f4 -> ResT $ Branch3 esq y (Branch2 f1 a f2) b (Branch2 f3 c f4)

insertB f x (Branch3 esq y mig z dreta)
        | x == y = ResT $ Branch3 esq (combineEntry f y x) mig z dreta
        | x == z = ResT $ Branch3 esq y mig (combineEntry f z x) dreta
        | x < y = case insertB f x esq of
                        ResT arb -> ResT $ Branch3 arb y mig z dreta
                        Branch4 f1 a f2 b f3 c f4 -> Branch4 (Branch2 f1 a f2) b (Branch2 f3 c f4) y mig z dreta
        | x < z = case insertB f x mig of
                        ResT arb -> ResT $ Branch3 esq y arb z dreta
                        Branch4 f1 a f2 b f3 c f4 -> Branch4 esq y (Branch2 f1 a f2) b (Branch2 f3 c f4) z dreta
        | otherwise = case insertB f x dreta of
                          ResT arb -> ResT $ Branch3 esq y mig z arb
                          Branch4 f1 a f2 b f3 c f4 -> Branch4 esq y mig z (Branch2 f1 a f2) b (Branch2 f3 c f4)


updateB :: Ord k => Entry k v -> Tree k v -> Tree k v
updateB _ Nil = Nil

updateB x ar @ (Branch2 Nil y Nil)
        | x == y = Branch2 Nil x Nil
        | otherwise = ar

updateB x ar @ (Branch3 Nil y Nil z Nil)
        | x == y = Branch2 Nil z Nil
        | x == z = Branch2 Nil y Nil
        | otherwise = ar

updateB x ar @ (Branch2 esq y dreta)
        | x == y = Branch2 esq x dreta
        | x < y = Branch2 (updateB x esq) y dreta
        | otherwise = Branch2 esq y (updateB x dreta)

updateB x ar @ (Branch3 esq y mig z dreta)
        | x == y = Branch3 esq x mig z dreta
        | x == z = Branch3 esq y mig x dreta
        | x < y = Branch3 (updateB x esq) y mig z dreta
        | x < z = Branch3 esq y (updateB x mig) z dreta
        | otherwise = Branch3 esq y mig z (updateB x dreta)

lookupB :: Ord k => k -> Tree k v -> Maybe (k, v)
lookupB k Nil = Nothing
lookupB k (Branch2 esq y dreta)
        | k == key y = E.toMaybe y
        | k < key y = lookupB k esq
        | otherwise = lookupB k dreta

lookupB k (Branch3 esq y mig z dreta)
        | k == key y = E.toMaybe y
        | k == key z = E.toMaybe z
        | k < key y = lookupB k esq
        | k < key z = lookupB k mig
        | otherwise = lookupB k dreta

containsB :: Ord k => k -> Tree k v -> Bool
containsB k t = isJust $ lookupB k t

deleteB :: Ord k => k -> Tree k v -> Tree k v
deleteB _ Nil = Nil

deleteB k ar @ (Branch2 esq y dreta)
        | k == key y = Branch2 esq (invalidate y) dreta
        | k < key y = Branch2 (deleteB k esq) y dreta
        | otherwise = Branch2 esq y (deleteB k dreta)

deleteB k ar @ (Branch3 esq y mig z dreta)
        | k == key y = Branch3 esq (invalidate y) mig z dreta
        | k == key z = Branch3 esq y mig (invalidate z) dreta
        | k < key y = Branch3 (deleteB k esq) y mig z dreta
        | k < key z = Branch3 esq y (deleteB k mig) z dreta
        | otherwise = Branch3 esq y mig z (deleteB k dreta)

toDList :: Tree k v -> D.DList (k, v)
toDList Nil = D.empty
toDList (Branch2 esq x dreta) = toDList esq `D.append` valToDList x `D.append` toDList dreta
toDList (Branch3 esq x mig y dreta) = toDList esq `D.append` valToDList x `D.append` toDList mig `D.append` valToDList y `D.append` toDList dreta

mapEntriesValues :: (Entry k v1 -> Entry k v2) -> Tree k v1 -> Tree k v2
mapEntriesValues f Nil = Nil
mapEntriesValues f (Branch2 esq x dreta) = Branch2 (mapEntriesValues f esq) (f x) (mapEntriesValues f dreta)
mapEntriesValues f (Branch3 esq x mig y dreta) = Branch3 (mapEntriesValues f esq) (f x) (mapEntriesValues f mig) (f y) (mapEntriesValues f dreta)

mapEntriesKeysMonotonic :: (Ord k1, Ord k2) => (Entry k1 v -> Entry k2 v) -> Tree k1 v -> Tree k2 v
mapEntriesKeysMonotonic f Nil = Nil
mapEntriesKeysMonotonic f (Branch2 esq x dreta) = Branch2 (mapEntriesKeysMonotonic f esq) (f x) (mapEntriesKeysMonotonic f dreta)
mapEntriesKeysMonotonic f (Branch3 esq x mig y dreta) = Branch3 (mapEntriesKeysMonotonic f esq) (f x) (mapEntriesKeysMonotonic f mig) (f y) (mapEntriesKeysMonotonic f dreta)

mapEntriesMonotonic :: (Entry k v -> Entry k v) -> Tree k v -> Tree k v
mapEntriesMonotonic f Nil = Nil
mapEntriesMonotonic f (Branch2 esq x dreta) = Branch2 (mapEntriesMonotonic f esq) (f x) (mapEntriesMonotonic f dreta)
mapEntriesMonotonic f (Branch3 esq x mig y dreta) = Branch3 (mapEntriesMonotonic f esq) (f x) (mapEntriesMonotonic f mig) (f y) (mapEntriesMonotonic f dreta)

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
firstOfMaybes xs = Safe.atDef Nothing (L.dropWhile isNothing xs) 0
