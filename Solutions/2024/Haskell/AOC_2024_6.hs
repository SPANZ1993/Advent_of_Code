import Data.List.Split (splitOn)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe, isNothing)
import System.TimeIt (timeIt)

inputToMap :: String -> Map.Map (Int, Int) Char
inputToMap inp =
  Map.fromList $
    concat $
      [zip (zip (replicate (length s) ln) [0 .. length s]) s | (s, ln) <- zip splitNewline [0 .. length splitNewline - 1]]
  where
    splitNewline = splitOn "\n" inp

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

data Direction = U | D | L | R deriving (Eq)

dir :: (Num a, Num b) => Direction -> (a, b)
dir d
  | d == U = (-1, 0)
  | d == L = (0, -1)
  | d == R = (0, 1)
  | d == D = (1, 0)

nextDirection :: Direction -> Direction
nextDirection d
  | d == U = R
  | d == R = D
  | d == D = L
  | d == L = U

traversePath :: Map.Map (Int, Int) Char -> (Int, Int) -> Direction -> [((Int, Int), Direction)] -> Map.Map (Int, Int) Char
traversePath m loc d hist
  | (loc, d) `elem` hist = Map.fromList [((0, 0), 'F')]
  | isNothing $ Map.lookup nextLoc m = Map.insert loc 'X' m
  | Map.lookup nextLoc m == Just '#' = traversePath m loc (nextDirection d) ((loc, d) : hist)
  | Map.lookup nextLoc m `elem` [Just '.', Just 'X'] = traversePath (Map.insert loc 'X' m) nextLoc d ((loc, d) : hist)
  | otherwise = Map.fromList [((0, 0), fromJust (Map.lookup nextLoc m))]
  where
    nextLoc = loc `add` dir d

findStartLoc :: Map.Map (Int, Int) Char -> (Int, Int)
findStartLoc m = head $ filter (\x -> Map.lookup x m == Just '^') (Map.keys m)

countLoops :: Map.Map (Int, Int) Char -> (Int, Int) -> Direction -> [(Int, Int)] -> Int
countLoops m loc d pairs = length $ filter (== 'F') $ map (\c -> head $ Map.elems (traversePath c loc d [])) candidates
  where
    candidates = [Map.insert pair '#' m | pair <- pairs]

-- maxi = maximum $ map fst $ Map.keys m
-- maxj = maximum $ map snd $ Map.keys m
-- pairs = [(i, j) | i <- [0 .. maxi], j <- [0 .. maxj]]

map2str :: Map.Map (Int, Int) Char -> [Char]
map2str m = unlines $ [[fromJust (Map.lookup (i, j) m) | j <- [0 .. maxj]] | i <- [0 .. maxi]]
  where
    maxi = maximum $ map fst $ Map.keys m
    maxj = maximum $ map snd $ Map.keys m

-- TODO: Make Part 2 go faster. The issue is that I need to 'teleport' from place to place instead of taking an iteration per movement tile.

main :: IO ()
main = do
  contents_ <- readFile "../../../Input/2024/6.txt"
  let contents = inputToMap contents_

  -- Part 1
  timeIt
    ( do
        let startLoc = findStartLoc contents
        let path = traversePath contents startLoc U []
        let positions = sum $ map (\c -> fromEnum (c == 'X')) (Map.elems path)
        putStrLn $ "Part 1: " ++ show positions
    )
  -- Part 2
  timeIt
    ( do
        let startLoc = findStartLoc contents
        let path = traversePath contents startLoc U []
        let traversedLocs = filter (\k -> Map.lookup k path == Just 'X') (Map.keys path)
        let nLoops = countLoops contents startLoc U traversedLocs
        putStrLn $ "Part 2: " ++ show nLoops
    )
