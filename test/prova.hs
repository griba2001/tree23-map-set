{-# LANGUAGE PackageImports #-}
import Data.Tree23.SortedSet as T
import Test.QuickCheck
import Data.List as L
-- import System.Random
-- import "monad-loops" Control.Monad.Loops
-- import Data.Maybe

prop :: [Int] -> Bool
prop xs = compare (toList arbre2) seqord == EQ
        where
                xs' = L.nub xs
                arbre = foldl (flip T.insert) T.empty xs'
                ys = L.drop (length xs' `div` 2) xs'
                seqord = L.sort (xs' \\ ys)
                arbre2 = foldl (flip T.delete) arbre ys

main :: IO ()                
main = quickCheck prop


{-
(.$) :: t -> (t -> r) -> r
x .$ f = f x

rollDice :: IO Int
rollDice = getStdRandom (randomR (1,60))

nextDice :: Int -> IO (Maybe (Int, Int))
nextDice cnt = if cnt > 10
                  then return Nothing
                  else do
                          x <- rollDice
                          return $ Just (x, cnt +1)

main = do
        xs <- unfoldrM nextDice 0
        let arbre = foldl (flip T.insert) T.empty xs
        putStrLn $ show arbre ++ "\n"
                -}
