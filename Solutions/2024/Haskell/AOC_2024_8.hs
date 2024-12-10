import Data.List.Split (splitOn)
import System.TimeIt (timeIt)

unique :: (Eq a) => [a] -> [a]
unique = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

inputToList :: [Char] -> (Int, Int, [(Char, [(Int, Int)])])
inputToList inp =
  (boundi, boundj, [(curLet, map fst $ filter (\x -> snd x == curLet) locChar) | curLet <- filter (/= '.') (unique (map snd locChar))])
  where
    splitNewline = splitOn "\n" inp
    boundi = maximum $ map (fst . fst) locChar
    boundj = maximum $ map (snd . fst) locChar
    locChar =
      concat $
        [zip (zip (replicate (length s) ln) [0 .. length s]) s | (s, ln) <- zip splitNewline [0 .. length splitNewline - 1]]

getAntinodes :: Int -> Int -> Bool -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
getAntinodes boundi boundj repeat (i1, j1) (i2, j2)
  | (i1, j1) == (i2, j2) = []
  | otherwise =
      concat
        [ filter
            (\(i, j) -> 0 <= i && i <= boundi && 0 <= j && j <= boundj)
            [(i1 + (slopei * n), j1 + (slopej * n)), (i2 - (slopei * n), j2 - (slopej * n))]
          | n <- (if repeat then [0 .. 1 + max boundi boundj] else [1])
        ]
  where
    slopei = i1 - i2
    slopej = j1 - j2

getAntinodesFrequency :: Int -> Int -> Bool -> [(Int, Int)] -> [(Int, Int)]
getAntinodesFrequency boundi boundj repeat antennas =
  unique
    ( concat $
        foldr (\x acc -> acc ++ map (getAntinodes boundi boundj repeat x) antennas) [] antennas
    )

main :: IO ()
main = do
  contents_ <- readFile "../../../Input/2024/8.txt"
  let (boundi, boundj, contents) = inputToList contents_

  -- Part 1
  timeIt
    ( do
        let antinodeLocs1 = length . unique $ concatMap (getAntinodesFrequency boundi boundj False . snd) contents
        putStrLn $ "Part 1: " ++ show antinodeLocs1
    )
  -- Part 2
  timeIt
    ( do
        let antinodeLocs2 = length . unique $ concatMap (getAntinodesFrequency boundi boundj True . snd) contents
        putStrLn $ "Part 2: " ++ show antinodeLocs2
    )
