module Day1 where

import Data.Char
import Data.List
import Data.Composition
import Data.Tuple.Extra

main :: IO ()
main = interact (show . solve . parse)

solve :: ([Int], [Int]) -> (Int, Int)
solve = both sum . (p1 &&& p2)

p1, p2 :: ([Int], [Int]) -> [Int]
p1 = uncurry (zipWith $ abs .: (-))
p2 = uncurry (zipWith (*))
   . (fst &&& uncurry (traverse $ length .: elemIndices))

parse :: String -> ([Int], [Int])
parse = both sort
      . unzip
      . map (both read . break isSpace)
      . lines


