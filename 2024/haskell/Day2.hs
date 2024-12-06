module Day2 where

import Data.List.Extra
import Data.Tuple.Extra

main :: IO ()
main = interact (show . solve . parse)

solve :: [[Int]] -> (Int, Int)
solve = p1 &&& p2

p1 :: [[Int]] -> Int
p1 = length . filter validate
p2 = length . filter (any validate . skip1)

parse :: String -> [[Int]]
parse = map (map read . words) . lines

-------------
--- Utils ---
-------------
validate :: [Int] -> Bool
validate = liftA2 (&&) checkOrdered checkGradual . zipAdj

checkOrdered, checkGradual :: [(Int, Int)] -> Bool
checkOrdered = (==) 1 . length . nubOrd . map (signum . uncurry (-))
checkGradual = all $ liftA2 (&&) (>= 1) (<= 3) . abs . uncurry (-)

zipAdj :: [a] -> [(a, a)]
zipAdj = zip <*> tail

skip1 :: [a] -> [[a]]
skip1 = liftA2 (zipWith (\i t -> i ++ (drop1 t))) inits tails
