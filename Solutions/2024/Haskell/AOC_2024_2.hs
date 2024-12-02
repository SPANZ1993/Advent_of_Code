import Data.List (sort)

readInts :: [String] -> [Int]
readInts [] = []
readInts x = map read x

isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted x = x == sort x

diffsBetween :: (Ord a, Num a) => [a] -> a -> a -> Bool
diffsBetween [] n1 n2 = True
diffsBetween [x] n1 n2 = True
diffsBetween (x1 : x2 : x) n1 n2 = d >= n1 && d <= n2 && diffsBetween (x2 : x) n1 n2
  where
    d = abs $ x1 - x2

filterSafe :: (Ord a, Num a) => a -> a -> [a] -> Bool
filterSafe n1 n2 x = (isSorted (reverse x) || isSorted x) && diffsBetween x n1 n2

removeAt :: [a] -> Int -> [a]
removeAt xs n = [xs !! i | i <- filter (/= n) [0 .. length xs - 1]]

main = do
  -- Prepping Data
  contents_ <- readFile "../../../Input/2024/2.txt"
  let contents = map words $ lines contents_
  let reports = map readInts contents
  -- print reports

  -- Part 1
  let nsafe =
        length $
          filter (filterSafe 1 3) reports
  putStrLn $ "Part 1: " ++ show nsafe

  -- Part 2
  let nsafe2 =
        length $
          filter
            ( \x ->
                or [filterSafe 1 3 (removeAt x i) | i <- [0 .. length x]]
            )
            reports
  putStrLn $ "Part 2: " ++ show nsafe2
