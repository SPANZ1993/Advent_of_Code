import Control.Monad
import Data.Char
import Data.List
import System.IO

readInts :: [String] -> [Int]
readInts [] = []
readInts x = map read x

calcSimScores :: (Eq a, Num a) => [a] -> [a] -> [a]
calcSimScores xs ys = map (\x -> x * sum [1 | y <- ys, x == y]) xs

main = do
  -- Prepping Data
  contents_ <- readFile "../../../Input/2024/1.txt"
  let contents = map words $ lines contents_
  let firstNums = readInts $ map (\(x : _) -> x) contents
  let secondNums = readInts $ map (\(_ : x : _) -> x) contents

  -- Part 1
  let diffs = zipWith (\x y -> abs $ x - y) (sort firstNums) (sort secondNums)
  let totalDiffs = sum diffs
  putStrLn $ "Part 1: " ++ show totalDiffs

  -- Part 2
  let simScore = sum $ calcSimScores firstNums secondNums
  putStrLn $ "Part 2: " ++ show simScore