module Day6 where

import Data.List.Extra

p1 = solve 4
p2 = solve 14

solve n = add n . length . takeWhile (s neq nub) . map (take n) . tails <$> readFile "d6" where
  add = (+)
  neq = (/=)
  s f g x = f x (g x)

-- solve n = applyN n succ . head . uncurry indices . bimap packet pack . w (,) <$> readFile "d6" where
--   w f x = f x x
--   packet = pack . head . filter ((==) n . length) . map nub . contSubSeqs
--   contSubSeqs = dropEnd (succ n) . map (take n) . tails
--   contSubSeqsOld = filter (not . null) . concatMap inits . tails
--   applyN = foldr (.) id .: replicate
