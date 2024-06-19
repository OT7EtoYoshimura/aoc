module Day4 where

import Data.List
import Data.List.Extra

solve f = length . filter id . map (f . pairs) . lines <$> readFile "d4" where
  pairs = map ranges . splitOn ","
  ranges :: [Char] -> [Integer]
  ranges = uncurry enumFromTo . toTuple . map read . splitOn "-"
  toTuple (x:y:_) = (x,y)

p1 = solve covers where
  covers = elem =<< foldr1 intersect
p2 = solve overlaps where
  overlaps = not . null . foldr1 intersect
