import Data.Char (isNumber)
import Data.List (group, splitAt)
import System.TimeIt (timeIt)

inputToList :: [Char] -> [Int]
inputToList inp = [read [c] | c <- filter isNumber inp]

expandDiskMap :: [Int] -> [Int]
expandDiskMap dm = concat [if even i then replicate n (i `div` 2) else replicate n (-1) | (n, i) <- zip dm [0 .. length dm - 1]]

splitExpandedDiskMap :: [Int] -> ([Int], [Int])
splitExpandedDiskMap edm = (forward, backward)
  where
    spliti = head [i | i <- [0 .. length edm], sum (map (fromEnum . (== (-1))) (take i edm)) == sum (map (fromEnum . (/= (-1))) (take (length edm - i) (reverse edm)))]
    forward = reverse $ take spliti edm
    backward = reverse $ filter (-1 /=) $ take (length edm - spliti) (reverse edm)

fillInSplitDiskMap :: [Int] -> ([Int], [Int]) -> [Int]
fillInSplitDiskMap out ([], []) = filter (-1 /=) out
fillInSplitDiskMap out (e1 : edm1, []) = fillInSplitDiskMap (e1 : out) (edm1, [])
fillInSplitDiskMap out (e1 : edm1, e2 : edm2)
  | e1 == -1 = fillInSplitDiskMap (e2 : out) (edm1, edm2)
  | otherwise = fillInSplitDiskMap (e1 : out) (edm1, e2 : edm2)

constructGroupedDisk :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
constructGroupedDisk out [] _ = reverse $ filter (/= []) out
constructGroupedDisk out (f1 : fw) [] = constructGroupedDisk (f1 : out) fw []
constructGroupedDisk out (f1 : fw) bw
  | (head f1 == (-1)) && null bwe = constructGroupedDisk (f1 : out) fw bw
  | head f1 == (-1) = constructGroupedDisk (bwe : out) (group $ concat (f1remainder : map (\e -> if e == bwe then replicate (length e) (-2) else e) fw)) (filter (/= bwe) bw)
  | otherwise = constructGroupedDisk (f1 : out) fw (filter (/= f1) bw)
  where
    bwe_candidates = filter (\e -> length e <= length f1) bw
    bwe = if null bwe_candidates then [] else head bwe_candidates
    f1remainder = replicate (length f1 - length bwe) (-1)

sumDiskMap :: [Int] -> Int
sumDiskMap dm = sum $ zipWith (*) dm [0 .. length dm - 1]

sumDiskMapGrouped :: [[Int]] -> Int
sumDiskMapGrouped dm = sum $ zipWith (\e i -> if e <= 0 then 0 else e * i) dm' [0 .. length dm' - 1]
  where
    dm' = concat dm

calculateChecksum :: [Int] -> Int
calculateChecksum dm = sumDiskMap $ fillInSplitDiskMap [] $ splitExpandedDiskMap $ expandDiskMap dm

calculateChecksum2 :: [Int] -> Int
calculateChecksum2 dm = sumDiskMapGrouped $ constructGroupedDisk [[]] (group $ expandDiskMap dm) (filter (\x -> head x /= (-1)) (group $ reverse $ expandDiskMap dm))

main :: IO ()
main = do
  contents_ <- readFile "../../../Input/2024/9.txt"
  let contents = inputToList contents_

  -- Part 1
  timeIt
    ( do
        let checkSum1 = calculateChecksum contents
        putStrLn $ "Part 1: " ++ show checkSum1
    )
  -- Part 2
  timeIt
    ( do
        let checkSum2 = calculateChecksum2 contents
        putStrLn $ "Part 2: " ++ show checkSum2
    )
