{-# LANGUAGE PackageImports, ExistentialQuantification #-}
import qualified Data.Tree23.SortedSet as S
import qualified Test.QuickCheck as Q
import qualified Data.List as L
import Control.Monad
import System.Exit (exitSuccess, exitFailure, exitWith, ExitCode(ExitFailure))

data MyProp = forall p. (Q.Testable p) => MyProp p  -- ExistentialQuantification

propInsertMember :: [Int] -> Bool
propInsertMember xs = L.all prop xs
  where
          -- tr :: Set Int  
          tr = S.insertAll xs S.empty
          -- prop :: Int -> Bool
          prop x = S.member x tr
        
propInsertDeleteNotMember :: [Int] -> Bool
propInsertDeleteNotMember xs = L.all prop xs
  where
          -- tr :: Set Int
          tr = S.insertAll  xs S.empty
          tr' = S.deleteAll xs tr
          prop x = not $ S.member x tr'

propSorted :: [Int] -> Bool
propSorted xs = compare (S.toList tr1) (L.sort . L.nub $ xs) == LT
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

props :: [MyProp]                
props = [MyProp propInsertMember,
         MyProp propInsertDeleteNotMember,
         MyProp propSorted,
         MyProp propSortedAfterDeletes]
-- props = [MyProp propInsertDeleteNotMember]


isSuccess :: Q.Result -> Bool
isSuccess (Q.Success _ _ _) = True
isSuccess _ = False

main :: IO Int
main = do
        results <- forM props $ \ (MyProp p) ->
                            Q.quickCheckResult p
        case L.all isSuccess results of
             True -> exitSuccess
             False -> exitFailure
