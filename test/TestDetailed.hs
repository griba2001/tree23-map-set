{-# LANGUAGE PackageImports, RecordWildCards, NamedFieldPuns #-}
module TestDetailed (tests) where

import qualified Test.QuickCheck as Q
import Distribution.TestSuite as TS

import TestSortedSet

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
                 Test $ TestInstance (runQuickCheck propSortedAfterDeletes) "propSortedAfterDeletes" ["set"] [] undefined,
                 Test $ TestInstance (runQuickCheck propMaximum) "propMaximum" ["set"] [] undefined,
                 Test $ TestInstance (runQuickCheck propMinimum) "propMinimum" ["set"] [] undefined
                 ]
