{-# LANGUAGE OverloadedStrings #-}
module Day05 where

import Control.Monad.Extra
import Data.List.Extra
import Data.Tuple.Extra
import Data.Composition
import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified TextShow     as T

main :: IO ()
main = T.interact ( T.showt
                  . solve
                  . parse
                  )

parse :: Text -> [Text]
parse = T.lines

solve :: [Text] -> (Int, Int)
solve = both (length . filter id) . unzip . map (p1 &&& p2)
                                              -- on (&&&) validate p1l p2l

p1 :: Text -> Bool
p1 = validate [cond1p1, cond2p1, cond3p1]
p2 = validate [cond1p2, cond2p2]

validate :: [a -> Bool] -> a -> Bool
validate = and .: sequenceA

cond1p1, cond2p1, cond3p1, cond1p2, cond2p2 :: Text -> Bool
cond1p1 = (<=) 3 . T.length . T.filter isVowel
cond2p1 = any ((1<) . T.length) . T.group
cond3p1 = not . anyM elem ["ab", "cd", "pq", "xy"] . windowsT 2
cond3p1v2 = not . or . traverse elem ["ab", "cd", "pq", "xy"] . windowsT 2
cond1p2 = any (liftA2 T.isInfixOf (T.take 2) (T.drop 2)) . dropEnd1 . T.tails
cond2p2 = any (liftA2 (==) fst3 thd3) . zip3Adj . T.unpack

isVowel :: Char -> Bool
isVowel = flip elem ("aeiou" :: [Char])

zip3Adj :: [a] -> [(a, a, a)]
zip3Adj = zip3 <*> drop 1 <*> drop 2

windowsT :: Int -> Text -> [Text]
windowsT n = T.transpose . take n . T.tails
