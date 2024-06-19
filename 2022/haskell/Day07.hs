{- I used vis(1) SRE's to generate the folder structure.
 - After that I ran this horrible program. -}
module Day7 where

import Control.Lens (unsnoc)
import Data.List
import System.Directory.Tree

data NewTree a = NewDir FileName [NewTree a] Integer
               | NewFile Integer

p1 = sum . filter (100000 >=) <$> solve

p2 = fmap (head . uncurry go) . unsnoc . sort <$> solve where
  go xs root = dropWhile (\x -> 70000000 - root + x <= 30000000) xs

solve = sizes . op . dirTree <$> readDirectoryWith return "day7/" where
  op (Dir n c) = NewDir n (map op c) (foldr further 0 $ concatMap flattenDir c)
  op (File n f) = NewFile $ read n
  further (File n f) acc = read n + acc
  further (Dir _ c) acc = acc
  sizes (NewDir _ xs size) = size : concatMap sizes xs
  sizes (NewFile _) = []
