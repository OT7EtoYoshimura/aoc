module Day5 where

import Data.Bifunctor
import Data.Char
import Data.List
import Data.List.Extra
import Data.List.Lens
import Control.Lens

p1 = solve reverse
p2 = solve id

solve f = map head . uncurry (move f) . bimap crates inst . breakOn "\n\n" <$> readFile "d5"

move _ crates [] = crates
move f crates ([count, from, to]:rest) = move f newCrates rest where
  taken = f $ take count $ crates !! pred from
  newCrates = crates & ix (pred from) %~ drop count & ix (pred to) %~ (++) taken

crates = filter (not . null) . map (filter isUpper) . splitOn "   "  . concat . transpose . lines . head . splitOn "\n 1"
inst = map (map (read :: String -> Int) . filter (all isDigit) . words) . lines . drop 2
