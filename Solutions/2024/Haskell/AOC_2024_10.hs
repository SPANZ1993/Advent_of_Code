import Data.HashSet qualified as Set
import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe, isNothing)
import System.TimeIt (timeIt)

inputToMap :: String -> Map.Map (Int, Int) Int
inputToMap inp = m
  where
    splitNewline = splitOn "\n" inp
    m =
      Map.fromList $
        concat $
          [zip (zip (replicate (length s) ln) [0 .. length s]) (map (\c -> read [c]) s) | (s, ln) <- zip splitNewline [0 .. length splitNewline - 1]]

data Direction = U | D | L | R deriving (Eq)

dir :: (Num a, Num b) => Direction -> (a, b)
dir d
  | d == U = (-1, 0)
  | d == L = (0, -1)
  | d == R = (0, 1)
  | d == D = (1, 0)

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

addDir :: (Int, Int) -> Direction -> (Int, Int)
addDir loc d = loc `add` dir d

incMaybe :: Maybe Int -> Maybe Int
incMaybe n
  | isNothing n = Nothing
  | otherwise = fmap (+ 1) n

scorePath :: Map.Map (Int, Int) Int -> Set.HashSet (Int, Int) -> (Int, Int) -> [Set.HashSet (Int, Int)]
scorePath m p loc
  | curVal /= Just 9 =
      concat
        [ if Map.lookup d m == incMaybe curVal then scorePath m (Set.insert loc p) d else []
          | d <- map (addDir loc) [U, L, R, D]
        ]
  | otherwise = [Set.insert loc p]
  where
    curVal = Map.lookup loc m

scoreAllPaths :: Map.Map (Int, Int) Int -> Bool -> Int
scoreAllPaths m p2 =
  sum
    [ length $
        (if p2 then id else nub) $
          map (head . filter (\loc -> Map.lookup loc m == Just 9) . Set.toList) (scorePath m Set.empty startLoc)
      | startLoc <- filter (\k -> Map.lookup k m == Just 0) (Map.keys m)
    ]

main :: IO ()
main = do
  contents_ <- readFile "../../../Input/2024/10.txt"
  let contents = inputToMap contents_

  -- Part 1
  timeIt
    ( do
        let npaths = scoreAllPaths contents False
        putStrLn $ "Part 1: " ++ show npaths
    )
  -- Part 2
  timeIt
    ( do
        let npaths2 = scoreAllPaths contents True
        putStrLn $ "Part 2: " ++ show npaths2
    )
