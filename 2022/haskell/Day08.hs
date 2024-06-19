module Day8 where

import Control.Monad
import Data.Char
import Data.List
import Data.Tuple.Extra
import GHC.Utils.Misc

p1 = length . filter id <$> solve (zipWith (||)) vis
p2 = maximum            <$> solve (zipWith  (*)) scenic

solve f g = transF f . both (revF f g) . transPair . digits . lines <$> readFile "d8"

digits    = map $ map digitToInt
transPair = id &&& transpose

transF  f = concatMap (uncurry f) . uncurry zip . second transpose
revF  f g = map $ liftM2 f (g mapAccumL) (g mapAccumR)

vis     f = snd . f (\max x -> if x > max then (x, True) else (max, False)) (-1)

scenic  f = snd . f (\his x -> (x:his, applyWhen (scenic' x his /= length his) succ $ scenic' x his)) []
scenic' x = length . takeWhile (x >)
