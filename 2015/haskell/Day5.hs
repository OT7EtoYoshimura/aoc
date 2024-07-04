{-# LANGUAGE OverloadedStrings #-}
module Day5 where

import Data.List
import Data.List.Extra
import Data.Tuple.Extra
import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified TextShow     as T

main :: IO ()
main = T.interact (T.showt . solve . parse)

parse :: Text -> [Text]
parse = T.lines

solve :: [Text] -> (Int, [Text])
solve = p1 &&& id

p1 :: [Text] -> Int
p1 = length . filter id . map validate

validate, cond1, cond2, cond3 :: Text -> Bool
validate = and . sequenceA [cond1, cond2, cond3]
cond1 = (<=) 3 . T.length . T.filter isVowel
cond2 = any (>1) . map T.length . T.group
cond3 = not . or . traverse elem ["ab", "cd", "pq", "xy"] . windowst 2

isVowel :: Char -> Bool
isVowel = flip elem ("aeiou" :: [Char])

zipAdj :: [a] -> [(a, a)]
zipAdj = zip <*> drop1

windowst :: Int -> Text -> [Text]
windowst n = T.transpose . take n . T.tails
