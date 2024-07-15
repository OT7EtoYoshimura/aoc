module Day where

import Data.Tuple.Extra

-- main :: IO ()
-- main = interact (show . solve . parse)

parse :: String -> String
parse = id

solve :: String -> (a, a)
solve = p1 &&& p2

p1, p2 :: String -> a
p1 = common
p2 = common

common :: String -> a
common = const undefined
