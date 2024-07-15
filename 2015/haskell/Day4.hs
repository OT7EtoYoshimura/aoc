module Day4 where

import Crypto.Hash
import Data.ByteString (ByteString)
import Data.Composition
import Data.List
import Data.String
import Data.Tuple.Extra

-- Make sure there's no trailing newline character at the end
main :: IO ()
main = interact ( show
                . solve
                . parse
                )

parse :: String -> [(Integer, String)]
parse key = map (toSnd $ md5 . (++) key . show) [1..]

solve :: [(Integer, String)] -> (Integer, Integer)
solve = p1 &&& p2

p1, p2 :: [(Integer, String)] -> Integer
p1 = common "00000"
p2 = common "000000"

common :: String -> [(Integer, String)] -> Integer
common pre = fst . findFst (isPrefixOf pre . snd)

findFst :: (a -> Bool) -> [a] -> a
findFst = head .: filter

md5 :: String -> String
md5 = show . (hash :: ByteString -> Digest MD5) . fromString

toSnd :: (a -> b) -> a -> (a, b)
toSnd = (&&&) id
