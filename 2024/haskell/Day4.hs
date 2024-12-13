module Day4 where

import Data.List.Extra
import Data.Tuple.Extra
import Data.Universe.Helpers

main :: IO ()
main = interact (show . solve . parse)

parse :: String -> [String]
parse = lines

solve :: [String] -> (Int, Int)
solve = p1  &&& p2

p1, p2 :: [String] -> Int
p1 = sum . map xmasCount . concat . transforms
p2 = foldr masCount 0 . submatrices 3

-------------
--- Utils ---
-------------
transforms :: [String] -> [[String]]
transforms = sequence [ id                                    -- rows
                      , map reverse                           -- rows     rev
                      , transpose                             -- cols
                      , map reverse . transpose               -- cols     rev
                      , diagonals                             -- off diag
                      , map reverse . diagonals               -- off diag rev
                      , diagonals . map reverse               -- diag
                      , map reverse . diagonals . map reverse -- diag     rev
                      ]

xmasCount :: String -> Int
xmasCount = flip go 0 where
  go []                    n =             n
  go ('X':'M':'A':'S':xs)  n = go xs (succ n)
  go (_:xs)                n = go xs       n

masCount :: [String] -> Int -> Int
masCount [['M',_,'M'],[_,'A',_],['S',_,'S']] n = succ n
masCount [['M',_,'S'],[_,'A',_],['M',_,'S']] n = succ n
masCount [['S',_,'M'],[_,'A',_],['S',_,'M']] n = succ n
masCount [['S',_,'S'],[_,'A',_],['M',_,'M']] n = succ n
masCount _                                   n =      n

submatrices :: Int -> [[a]] -> [[[a]]]
submatrices k matrix = [ map (take k . drop j) (take k (drop i matrix))
                      | i <- [0 .. n - k], j <- [0 .. n - k]
                      ] where n = length matrix

-- windows :: Int -> [a] -> [[a]]
-- windows n = dropEnd (pred n) . transpose . take n . tails
