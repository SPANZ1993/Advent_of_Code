import Data.Char (isDigit)
import Data.List.Split (splitOn)
import System.TimeIt (timeIt)
import Text.Regex.TDFA (AllTextMatches (getAllTextMatches), (=~), (=~~))

readInts :: [String] -> [Int]
readInts [] = []
readInts x = map read x

extractNumsMulStr :: String -> [Int]
extractNumsMulStr ms = readInts $ splitOn "," $ filter (\x -> isDigit x || x == ',') ms

mulre :: String
mulre = "mul\\([0-9]+,[0-9]+\\)"

main :: IO ()
main = do
  -- Prepping Data
  contents_ <- readFile "../../../Input/2024/3.txt"
  let contents = [c | c <- contents_, c /= '\n']

  -- Part 1
  timeIt
    ( do
        let mulstrs = getAllTextMatches $ contents =~ mulre :: [String]
        let sumInstructs1 = sum $ map (product . extractNumsMulStr) mulstrs
        putStrLn $ "Part 1: " ++ show sumInstructs1
    )
  -- Part 2
  timeIt
    ( do
        let filteredInstructions = concatMap (head . splitOn "don't()") $ splitOn "do()" contents
        let mulstrs = getAllTextMatches $ filteredInstructions =~ mulre :: [String]
        let sumInstructs2 = sum $ map (product . extractNumsMulStr) mulstrs
        putStrLn $ "Part 2: " ++ show sumInstructs2
    )
