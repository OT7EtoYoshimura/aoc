module Day10 where

import Data.List
import Data.List.Extra

p1 = sum . zipWith (*) indices . traverse nth indices <$> parse
p2 = mapM_ (putStrLn . zipWith locate [0..]) . chunksOf 40 =<< parse

parse = init . scanl' (+) 1 . concatMap transmute . lines <$> readFile "d10"

transmute "noop" = [0]
transmute ('a':'d':'d':'x':' ':rest) = [0, read rest]

locate r s = if abs (r - s) <= 1 then '#' else '.'

nth n xs = xs !! pred n
indices = [20,60..220]
