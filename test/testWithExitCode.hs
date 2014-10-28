{-# LANGUAGE PackageImports, ExistentialQuantification #-}
import qualified Test.QuickCheck as Q
import qualified Data.List as L
import Control.Monad
import System.Exit (exitSuccess, exitFailure, exitWith, ExitCode(ExitFailure))

import TestSortedSet

data MyProp = forall p. (Q.Testable p) => MyProp p  -- ExistentialQuantification


props :: [MyProp]                
props = [MyProp propInsertMember,
         MyProp propDeleteAfterInsertImpliesNotMember,
         MyProp propSorted,
         MyProp propSortedAfterDeletes,
         MyProp propYieldsOrigin,
         MyProp propMaximum,
         MyProp propMinimum        
                 ]


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
