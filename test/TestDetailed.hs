{-# LANGUAGE PackageImports, RecordWildCards, NamedFieldPuns #-}
module TestDetailed (tests) where

import Prelude hiding ((.))
import qualified Data.Tree23.SortedSet as S
import qualified Test.QuickCheck as Q
import qualified Data.List as L
import Control.Monad
import Control.Category as C
import Distribution.TestSuite as TS


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

-------------------------------------------------------                

toTSResult :: Q.Result -> TS.Result
toTSResult Q.Success {} = TS.Pass
toTSResult Q.GaveUp {} = TS.Fail "GaveUp"
toTSResult Q.Failure {reason} = TS.Fail reason

runQuickCheck :: Q.Testable p => p -> IO TS.Progress
runQuickCheck prop = do
                     qres <- Q.quickCheckResult prop
                     return $ (Finished . toTSResult) qres

tests :: IO [Test]
tests = return [ Test $ TestInstance (runQuickCheck propInsertMember) "propInsertMember" ["set"] [] undefined,
                 Test $ TestInstance (runQuickCheck propDeleteAfterInsertImpliesNotMember) "propDeleteAfterInsertImpliesNotMember" ["set"] [] undefined,
                 Test $ TestInstance (runQuickCheck propSorted) "propSorted" ["set"] [] undefined,
                 Test $ TestInstance (runQuickCheck propYieldsOrigin) "propYieldsOrigin" ["set"] [] undefined,        
                 Test $ TestInstance (runQuickCheck propSortedAfterDeletes) "propSortedAfterDeletes" ["set"] [] undefined]
