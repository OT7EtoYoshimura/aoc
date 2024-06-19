module Day3 where

import Data.Char
import Data.List
import Data.List.Split
import Control.Applicative

priority x
  | isUpper x = ord x - 38
  | otherwise = ord x - 96

p1 = sum . map go . lines <$> readFile "d3" where
  go = priority . fst . head . filter (uncurry (==)) . cartProd . halves
  halves xs  = splitAt (length xs `div` 2) xs
  cartProd = uncurry $ liftA2 (,)

p2 = sum . map go . chunksOf 3 . lines <$> readFile "d3" where
  go = priority . head . foldr1 intersect

-- 200 IQ alternative; although slower
-- priority = succ . head . flip elemIndices (['a' .. 'z'] ++ ['A' .. 'Z'])
