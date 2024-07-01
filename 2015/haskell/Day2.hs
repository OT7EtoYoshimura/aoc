{-# Language OverloadedStrings #-}

module Day2 where

import Data.Either
import Data.List
import Data.Tuple.Extra
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

type Helper = [Integer] -> Integer

main :: IO ()
main = T.interact (T.pack . show . solve)

solve :: Text -> (Integer, Integer)
solve = (p1 *** p2) . dupe . map parse . T.lines

parse :: Text -> [Integer]
parse = rights . map (fmap fst . T.decimal) . T.splitOn "x"

p1, p2 :: [[Integer]] -> Integer
p1 = common area smallestSide
p2 = common ribbon bow

common :: Helper -> Helper -> [[Integer]] -> Integer
common f g = foldr1 (+) . map (liftA2 (+) f g)

area, smallestSide, ribbon, bow :: Helper
area [l,w,h] = 2*l*w + 2*w*h + 2*h*l
smallestSide = foldr1 (*)                  . tail . sortBy (flip compare)
ribbon       = foldr1 (+) . take 4 . cycle . tail . sortBy (flip compare)
bow          = foldr1 (*)
