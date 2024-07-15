{-# LANGUAGE OverloadedStrings #-}
module Day02 where

import Data.Either
import Data.List
import Data.Tuple.Extra
import Data.Text (Text)
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import qualified Data.Text.Read as T
import qualified TextShow       as T

type Solver = [[Integer]] -> Integer
type Helper =  [Integer]  -> Integer

main :: IO ()
main = T.interact ( T.showt
                  . solve
                  . parse
                  )

parse :: Text -> [[Integer]]
parse = map line . T.lines where
  line = rights . map (fmap fst . T.decimal) . T.splitOn "x"

solve :: [[Integer]] -> (Integer, Integer)
solve = p1 &&& p2

p1, p2 :: Solver
p1 = common area smallestSide
p2 = common ribbon bow

common :: Helper -> Helper -> Solver
common f g = sum . map (liftA2 (+) f g)

area, smallestSide, ribbon, bow :: Helper
area [l,w,h] = 2*l*w + 2*w*h + 2*h*l
smallestSide = product              . take 2 . sort
ribbon       = sum . take 4 . cycle . take 2 . sort
bow          = product
