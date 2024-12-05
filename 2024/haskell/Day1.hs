module Day1 where

import Data.List
import Data.Composition
import Data.Tuple.Extra

main :: IO ()
main = interact (show . solve . parse)

solve :: [[Int]] -> (Int, Int)
solve = both sum . (p1 &&& p2)

p1, p2 :: [[Int]] -> [Int]
p1 = \[x, y] -> zipWith (abs .: (-)) x y
p2 = uncurry (zipWith (*))
   . (head &&& (\[x, y] -> traverse (length .: elemIndices) x y))

parse :: String -> [[Int]]
parse = map (sort . map read)
       . transpose
       . map words
       . lines
