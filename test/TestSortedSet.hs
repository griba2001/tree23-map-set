module TestSortedSet where

import Prelude hiding ((.))
import qualified Data.Tree23.SortedSet as S
import qualified Data.List as L
import Control.Monad
import Control.Category as C
import Data.Maybe

propInsertMember :: [Int] -> Bool
propInsertMember xs = L.all prop xs
  where
          -- tr :: Set Int
          tr = S.insertAll xs S.empty
          -- prop :: Int -> Bool
          prop x = S.member x tr

(.$) :: a -> (a -> b) -> b
x .$ f = f x

propDeleteAfterInsertImpliesNotMember :: [Int] -> Bool
propDeleteAfterInsertImpliesNotMember xs = L.all (\x -> S.notMember x set) xs
  where
          set = S.empty
                .$ S.insertAll xs
                .$ S.deleteAll xs

propSorted :: [Int] -> Bool
propSorted xs = compare (S.toList tr1) (xs .$ (L.nub >>> L.sort)) == EQ  -- (L.sort . L.nub $ xs)
        where
                tr1 = S.insertAll xs S.empty

propSortedAfterDeletes :: [Int] -> Bool
propSortedAfterDeletes xs = compare (S.toList tr2) orderedSeq == EQ
        where
                tr1 = S.insertAll xs S.empty
                xs' = L.nub xs
                ys = L.drop (length xs' `div` 2) xs'
                orderedSeq = L.sort (xs' L.\\ ys)
                tr2 = S.deleteAll ys tr1

propYieldsOrigin :: [Int] -> Bool
propYieldsOrigin xs = compare (S.toList . S.fromList $ xs) (L.sort . L.nub $ xs) == EQ

propMaximum :: [Int] -> Bool
propMaximum [] = True
propMaximum xs = maxSet == maximum xs
  where
    Just maxSet = S.findMax $ S.fromList xs

propMinimum :: [Int] -> Bool
propMinimum [] = True
propMinimum xs = minSet == minimum xs
  where
    Just minSet = S.findMin $ S.fromList xs

-------------------------------------------------------

