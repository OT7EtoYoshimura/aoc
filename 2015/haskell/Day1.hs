module Day1 where

import Data.Maybe
import Data.Tuple.Extra
import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified TextShow     as T

main :: IO ()
main = T.interact (T.showt . solve)

solve :: Text -> (Integer, Integer)
solve = p1 &&& p2 . T.unpack

p1 :: Text -> Integer
p1 = T.foldr selector 0

p2 :: String -> Integer
p2 = length . takeWhile (>=0) . scanl (flip selector) 0

selector :: Char -> Integer -> Integer
selector = fromMaybe id . flip lookup [('(', succ), (')', pred)]
