module Day06 where

import Data.List.Extra
import Data.Tuple.Extra
import Data.Maybe

type Coords = (Int, Int) -- (x, y)

main :: IO ()
main = interact (show . solve . parse)

parse :: a -> a
parse = undefined

solve :: a -> (a, a)
solve = p1 &&& p2

p1, p2 :: a -> a
p1 = undefined
p2 = undefined

-------------
--- Utils ---
-------------
parseCoords :: [String] -> [Coords]
parseCoords = concat . zipWithFrom (map . flip (,)) 0 . map (elemIndices '#')

parseGuard :: [String] -> Coords
parseGuard = head . catMaybes . zipWith (liftA2 $ flip (,)) (map Just [0..]) . map (elemIndex '^')
