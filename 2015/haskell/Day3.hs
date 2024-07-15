module Day3 where

import Data.List
import Data.List.Extra
import Data.Tuple.Extra
import Data.Maybe

main :: IO ()
main = interact ( show
                . solve
                -- . parse
                )

solve :: String -> (Int, Int)
solve = both cntUniq . (coordify &&& (uncurry (++) . both coordify . zipAdj))

p1 :: String -> Int
p1 = cntUniq . coordify

p2 :: String -> Int
p2 = cntUniq . uncurry (++) . both coordify . zipAdj

cntUniq :: Ord a => [a] -> Int
cntUniq = length . nubOrd

coordify :: String -> [(Int, Int)]
coordify = scanr dir (0,0)

dir :: (Enum a, Enum b) => Char -> (a, b) -> (a, b)
dir = fromMaybe id . flip lookup [ ('>', first  succ)
                                 , ('<', first  pred)
                                 , ('^', second succ)
                                 , ('v', second pred)
                                 ]

zipAdj :: [a] -> ([a], [a])
zipAdj = both (map snd) . partition (even . fst) . zip [0..]
