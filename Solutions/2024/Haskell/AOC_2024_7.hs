import Control.Monad (replicateM)
import Data.List.Split (split, splitOn)
import System.TimeIt (timeIt)

inputToList :: String -> [(Int, [Int])]
inputToList inp =
  [ (read v :: Int, map read (splitOn " " (head ops)))
    | (v : ops) <- map (splitOn ": ") (splitOn "\n" inp)
  ]

sumTup :: (Int, Int) -> Int
sumTup (x, y) = x + y

mulTup :: (Int, Int) -> Int
mulTup (x, y) = x * y

concatTup :: (Int, Int) -> Int
concatTup (x, y) = read (show x ++ show y)

checkEquation :: [(Int, Int) -> Int] -> (Int, [Int]) -> Bool
checkEquation _ (_, []) = False
checkEquation _ (val, [operand]) = val == operand
checkEquation operators (val, operands) = or [checkOperators val operands curOperators | curOperators <- replicateM (length operands - 1) operators]
  where
    checkOperators val operands curOperators
      | val == foldl (\acc (operand, operator) -> operator (acc, operand)) (head operands) (zip (tail operands) curOperators) = True
      | otherwise = False

main :: IO ()
main = do
  contents_ <- readFile "../../../Input/2024/7.txt"
  let contents = inputToList contents_
  timeIt
    ( do
        let calibrationRes1 = sum . map fst $ filter (checkEquation [sumTup, mulTup]) contents
        putStrLn $ "Part 1: " ++ show calibrationRes1
    )
  timeIt
    ( do
        let calibrationRes2 = sum . map fst $ filter (checkEquation [sumTup, mulTup, concatTup]) contents
        putStrLn $ "Part 2: " ++ show calibrationRes2
    )
