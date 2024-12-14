module Day05 where

import Data.List.Extra
import Data.Tuple.Extra
import Data.Maybe
import Data.Bifunctor

main :: IO ()
main = interact (show . solve . parse)

parse :: String -> ([[String]], [[String]])
parse = bimap parseRul parseUps . fromJust . stripInfix [""] . lines
  where parseUps = map $           splitOn ","
        parseRul = map $ reverse . splitOn "|"

solve :: ([[String]], [[String]]) -> (Int, Int)
solve = both (sum . map (read . middle)) . (p1 &&& p2)
  where middle xs = fromJust $ listToMaybe $ drop (length xs `div` 2) xs

p1, p2 :: ([[String]], [[String]]) -> [[String]]
p1 (r, u) =               filter (not . or . traverse isSubsequenceOf r) u
p2 (r, u) = map sorting $ filter (      or . traverse isSubsequenceOf r) u
  where sorting = sortBy $ \x y -> if [x,y] `elem` r then GT else LT
