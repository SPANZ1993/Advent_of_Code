import Data.Data (typeOf)
import Data.List (nub, sort)
import Data.List.Split (splitOn)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import GHC.Event.Windows.FFI (overlappedIONumBytes)
import System.TimeIt (timeIt)
import Universum.Nub (ordNub)

inputToList :: String -> [[Int]]
inputToList inp = map (\x -> [read x]) (splitOn " " inp)

applyRulesSingle :: [Int] -> [Int]
applyRulesSingle [] = []
applyRulesSingle [stone]
  | stone == 0 = [1]
  | even strLen = [read $ take (strLen `div` 2) strStone, read $ take (strLen `div` 2) (drop (strLen `div` 2) strStone)]
  | otherwise = [stone * 2024]
  where
    strStone = show stone
    strLen = length strStone

applyRules :: Int -> [[Int]] -> [[Int]]
applyRules n stones
  | n == 0 = stones
  | otherwise = applyRules ((-1) + n) (map (: []) $ concatMap applyRulesSingle stones)

insertMany :: (Ord a1) => Map.Map a1 a2 -> [(a1, a2)] -> Map.Map a1 a2
insertMany m es = foldl (\m e -> Map.insert (fst e) (snd e) m) m es

-- applyRulesCached :: Map.Map (Int, Int) [Int] -> Int -> Int -> Int -> [[Int]] -> [[Int]]
-- applyRulesCached :: Map.Map (Int, Int) [Int] -> Int -> Int -> Int -> [[Int]] -> ([((Int, Int), [Int])], Map.Map (Int, Int) [Int])
-- applyRulesCached :: Map.Map (Int, Int) [Int] -> Int -> Int -> Int -> [[Int]] -> [[Int]]
applyRulesCached :: Map.Map (Int, Int) [Int] -> Int -> Int -> Int -> [[Int]] -> Int
applyRulesCached c n itn totn stones
  -- \| itn == totn = stones
  | itn == totn - n = sum $ map (\stone -> length $ fromJust (Map.lookup (head stone, n) nextC)) stones
  | otherwise =
      applyRulesCached
        c
        n
        (itn + n)
        totn
        ( map (: []) $
            concat $
              map
                ( \stone ->
                    fromJust (Map.lookup (head stone, n) nextC)
                )
                stones
        )
  where
    nextCachedVals = [((head stone, n), map head $ applyRules n [stone]) | stone <- ordNub stones, not (Map.member (head stone, n) c)]
    nextC = insertMany c nextCachedVals

main :: IO ()
main = do
  contents_ <- readFile "../../../Input/2024/11.txt"
  let contents = inputToList contents_

  timeIt
    ( do
        print $ length $ sort $ ordNub $ applyRules 25 contents
        -- print $ length $ applyRules 25 $ sort $ ordNub $ applyRules 25 contents
        -- print $ length $ ordNub $ applyRules 25 $ sort $ ordNub $ applyRules 25 contents
    )

  -- timeIt
  --   ( do
  --       print $ length $ map (: []) $ concat $ map (\x -> [x + 1]) [0 .. 1000000000]
  --   )

  -- -- print $ insertMany (Map.fromList [(5, 6), (7, 8)]) [(1, 2), (3, 4)]

  -- let stones = applyRulesCached Map.empty 25 0 50 contents
  -- print $ length stones
  let r1 = [applyRules 25 [c] | c <- contents]
  let m1 = Map.fromList (zip contents r1)
  let r2 = [if Map.member c m1 then fromJust (Map.lookup c m1) else applyRules 25 [c] | c <- concat r1]
  -- let m2 = Map.fromList (ordNub (zip (contents ++ concat r1) (r1 ++ r2)))
  -- let m2 = Map.fromList (ordNub (zip (concat r1) r2))
  let m2 = Map.fromList (zip (concat r1) r2)
  let lm1 = Map.map length m1 :: Map.Map [Int] Int
  let lm2 = Map.map length m2 :: Map.Map [Int] Int
  let r3 = sum $ [sum $ [if Map.member c lm2 then fromJust (Map.lookup c lm2) else if Map.member c lm1 then fromJust (Map.lookup c lm1) else length $ applyRules 25 [c] | c <- l] | l <- r2]
  -- print $ sum r3

  print $ contents

  print $ length r1

-- print $ length $ Map.keys m1
-- print $ length $ concat r1
-- print $ length r2
-- print $ length $ Map.keys m2
-- print $ length $ Map.keys lm1
-- print $ length $ Map.keys lm2
-- -- print $ sum $ map length r2
-- print $ length $ ordNub $ take 100 r2

-- print $ sum $ map length [[c + 1 | c <- l] | l <- [[1, 2, 3], [4, 5], [6], [7, 8, 9]]]

-- print $ head r2

-- print $ take 10 $ concat r1
-- print $ take 10 $ Map.keys m1

-- print $ [Map.lookup k m1 | k <- take 1 $ Map.keys m1]

-- print $ take 10 $ concat r2
-- print $ length $ concat r2
-- print $ take 10 $ Map.keys m2
-- print $ [Map.lookup k m2 | k <- take 10 $ Map.keys m2]

-- print $ (zip (contents ++ concat r1) (concat r1 ++ concat r2))

-- let r2_nub = ordNub r2
-- print $ head r2

-- print r2

-- print nextC

-- print $ length stones

-- print $ map head $ applyRules 25 [[554735]]
