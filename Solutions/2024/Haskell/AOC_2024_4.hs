import Data.List.Split (splitOn)
import Data.Map qualified as Map

thd :: (a, b, c) -> c
thd (_, _, z) = z

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

perms2 :: [a] -> [(a, a)]
perms2 [] = []
perms2 xs = concat [[(e1, e2) | e1 <- xs] | e2 <- xs]

inputToMap :: String -> Map.Map (Int, Int) Char
inputToMap inp =
  Map.fromList $
    concat $
      [zip (zip (replicate (length s) ln) [0 .. length s]) s | (s, ln) <- zip splitNewline [0 .. length splitNewline - 1]]
  where
    splitNewline = splitOn "\n" inp

dirsToCheck :: String -> [[(Int, Int)]]
dirsToCheck "" = []
dirsToCheck q =
  map (uncurry zip) $
    perms2
      [ replicate (length q) 0,
        [0 .. length q - 1],
        map (* (-1)) [0 .. length q - 1]
      ]

checkWord' :: (Int, Int) -> [(Int, Int)] -> Map.Map (Int, Int) Char -> String -> Bool
checkWord' loc offsets doc query = and $ zipWith (==) foundChars $ map Just query
  where
    foundChars =
      [ Map.lookup (i, j) doc
        | (i, j) <-
            zipWith
              (\(i', j') (oi, oj) -> (i' + oi, j' + oj))
              (replicate (length offsets) loc)
              offsets
      ]

checkWord :: String -> Map.Map (Int, Int) Char -> Int
checkWord query doc =
  sum $
    map fromEnum $
      [checkWord' (i, j) offsets doc query | i <- [0 .. maxi], j <- [0 .. maxj], offsets <- dirs]
  where
    dirs = dirsToCheck query
    maxi = maximum $ map fst $ Map.keys doc
    maxj = maximum $ map snd $ Map.keys doc

masPat :: [[((Int, Int), Char)]]
masPat =
  map
    (++ [((0, 0), 'A')])
    [ zip (perms2 [-1, 1]) "MMSS",
      zip (perms2 [-1, 1]) "MSMS",
      zip (perms2 [-1, 1]) "SMSM",
      zip (perms2 [-1, 1]) "SSMM"
    ]

checkPat' :: (Int, Int) -> [((Int, Int), Char)] -> Map.Map (Int, Int) Char -> Bool
checkPat' loc pat doc = and $ zipWith (==) foundChars (map (Just . snd) pat)
  where
    foundChars =
      [ Map.lookup (i, j) doc
        | (i, j) <-
            zipWith
              (\(i', j') (oi, oj) -> (i' + oi, j' + oj))
              (replicate (length pat) loc)
              (map fst pat)
      ]

checkPat :: [[((Int, Int), Char)]] -> Map.Map (Int, Int) Char -> Int
checkPat pat doc =
  sum $
    map fromEnum $
      [checkPat' (i, j) cur_pat doc | i <- [0 .. maxi], j <- [0 .. maxj], cur_pat <- pat]
  where
    maxi = maximum $ map fst $ Map.keys doc
    maxj = maximum $ map snd $ Map.keys doc

-- TODO: Build and XMAS pattern so part 1 can be solved with checkPat

main :: IO ()
main = do
  -- Prepping Data
  contents_ <- readFile "../../../Input/2024/4.txt"
  let contents = inputToMap contents_

  -- Part 1
  let matches1 = checkWord "XMAS" contents
  putStrLn $ "Part 1: " ++ show matches1

  -- Part 2
  let matches2 = checkPat masPat contents
  putStrLn $ "Part 2: " ++ show matches2