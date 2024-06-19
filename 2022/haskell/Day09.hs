module Day9 where

import Data.Bifunctor
import Data.List.Extra
import GHC.Utils.Misc

p1 = solve 1
p2 = solve 9

solve n = length . nub . nTimes n (scanl1 follow) . move . concatMap (expand . second read . word1) . lines <$> readFile "d9"

expand = uncurry $ flip replicate

move     = scanl' (flip step) (0,0)
step "R" = first  succ
step "U" = second succ
step "L" = first  pred
step "D" = second pred

follow t@(tx, ty) (hx, hy) = (nx, ny) where
  dx = hx - tx
  dy = hy - ty
  (nx, ny) = if dx^2 + dy^2 <= 2 then t else (tx + signum dx, ty + signum dy)
