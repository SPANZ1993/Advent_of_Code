import Data.List
import Data.List.Split (splitOn)
import Data.Map qualified as Map
import System.TimeIt (timeIt)

perms2 :: [a] -> [(a, a)]
perms2 [] = []
perms2 xs = concat [[(e1, e2) | e1 <- xs] | e2 <- xs]

checkUpdate' :: [(Int, Int)] -> [Int] -> Bool
checkUpdate' _ [] = True
checkUpdate' fmap (x : xs) = all (\e -> x == e || elem (x, e) fmap) xs && checkUpdate' fmap xs

checkUpdate :: [(Int, Int)] -> [[Int]] -> [Bool]
checkUpdate fmap us = [checkUpdate' fmap u | u <- us]

filteredMiddleSum :: [Bool] -> [[Int]] -> Int
filteredMiddleSum mask updates = sum $ map ((\x -> x !! (length x `div` 2)) . snd) (filter fst (zip mask updates))

fixOrder' :: [(Int, Int)] -> [Int] -> [Int]
fixOrder' fmap u =
  map snd $
    sortBy (\a b -> compare (fst a) (fst b)) $
      [ (nmatch, ce)
        | (nmatch, ce) <-
            zip
              ( map
                  ( \e ->
                      sum $ map (fromEnum . (\x -> (x, e) `elem` fmap)) u
                  )
                  u
              )
              u
      ]

fixOrder :: [(Int, Int)] -> [[Int]] -> [[Int]]
fixOrder fmap = map (fixOrder' fmap)

getIncorrectUpdates :: [Bool] -> [[Int]] -> [[Int]]
getIncorrectUpdates mask us = map snd $ filter fst (zip (map not mask) us)

main :: IO ()
main = do
  -- Prepping Data
  contents_ <- readFile "../../../Input/2024/5.txt"
  let ruleStrs = filter (isInfixOf "|") (splitOn "\n" contents_)
  let updateStrs = filter (isInfixOf ",") (splitOn "\n" contents_)
  let ruleTuples = [(read k, read v) | [k, v] <- map (splitOn "|") ruleStrs] :: [(Int, Int)]
  let updateLists = [map read e | e <- map (splitOn ",") updateStrs] :: [[Int]]

  -- Part 1
  timeIt
    ( do
        let mask = checkUpdate ruleTuples updateLists
        let pageNumSum1 = filteredMiddleSum mask updateLists
        putStrLn $ "Part 1: " ++ show pageNumSum1
    )
  -- Part 2
  timeIt
    ( do
        let mask = checkUpdate ruleTuples updateLists
        let updateListsBad = getIncorrectUpdates mask updateLists
        let fixedUpdateList = fixOrder ruleTuples updateListsBad
        let pageNumSum2 = filteredMiddleSum (replicate (length fixedUpdateList) True) fixedUpdateList
        putStrLn $ "Part 2: " ++ show pageNumSum2
    )
