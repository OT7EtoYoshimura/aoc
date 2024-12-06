module Day2 where

import Data.List
import Data.List.Extra

-- main :: IO ()
-- main = interact (show . solve . parse)

p1 :: [[(Int, Int)]] -> Int
p1 = length . filter (liftA2 (&&) checkOrdered checkGradual)

parse :: String -> [[(Int, Int)]]
parse = map (zipAdj . map read . words) . lines

checkOrdered, checkGradual :: [(Int, Int)] -> Bool
checkOrdered = (==) 1 . length . nubOrd . map (signum . uncurry (-))
checkGradual = all $ liftA2 (&&) (>= 1) (<= 3) . abs . uncurry (-)

zipAdj :: [a] -> [(a, a)]
zipAdj = zip <*> tail
