{-# LANGUAGE PackageImports, NamedFieldPuns #-}
module Data.Tree23.Entry where

import Data.Maybe
import Data.Ord
import qualified "dlist" Data.DList as D
import Data.Monoid (Monoid, mempty, mappend, (<>), mconcat)

-- import Data.Tree23.Pair

data Valid = Valid | Invalid deriving (Eq, Show)

data Entry k v = Entry {key::k, val::v, valid::Valid}  deriving Show

instance Eq k => Eq (Entry k v) where
  (Entry x _ _) == (Entry y _ _) = x == y

entryItem :: Entry k v -> (k, v)
entryItem e @ Entry {key, val} = (key, val)

instance Ord k => Ord (Entry k v) where
  compare = comparing key

valToDList :: Entry k v -> D.DList (k, v)
valToDList (Entry k v Valid) = D.singleton (k, v)
valToDList (Entry _ _ Invalid) = D.empty

toMaybe :: Entry k v -> Maybe (k, v)
toMaybe (Entry k v Valid) = Just (k, v)
toMaybe (Entry _ _ Invalid) = Nothing

isValid :: Entry k v -> Bool
isValid (Entry _ _ Valid) = True
isValid (Entry _ _ Invalid) = False

invalidate :: Entry k v -> Entry k v
invalidate e = e { valid = Invalid}

-- combine entry values of entries with same key
combineEntry :: Eq k => (v -> v -> v) -> Entry k v -> Entry k v -> Entry k v
combineEntry f (Entry k1 v1 Valid) (Entry k2 v2 w)
        | k1 == k2 = Entry k1 (f v1 v2) w
        
combineEntry f (Entry k1 v1 Invalid) e2 @ (Entry k2 v2 w)
        | k1 == k2 = e2

mapEntryValue :: (a -> b) -> Entry k a -> Entry k b
mapEntryValue f (Entry k v w) = Entry k (f v) w

mapEntryKey :: (k1 -> k2) -> Entry k1 v -> Entry k2 v
mapEntryKey f (Entry k v w) = Entry (f k) v w

filterEntry :: (k -> Bool) -> Entry k v -> Entry k v
filterEntry prop e @ (Entry _ _ Invalid) = e
filterEntry prop e @ (Entry k v Valid) = if prop k then e else Entry k v Invalid

-- used to get intersection from difference
flipEntryValid :: Entry k v -> Entry k v
flipEntryValid (Entry k v Valid) = Entry k v Invalid
flipEntryValid (Entry k v Invalid) = Entry k v Valid

{-
foldEntryKey :: (Monoid m) => (k -> m) -> Entry k v -> m
foldEntryKey f e = if isValid e then (f . key $ e) else mempty

foldEntryVal :: (Monoid m) => (a -> m) -> Entry a -> m
foldEntryVal f e = if isValid e then (f . val $ e) else mempty
-}