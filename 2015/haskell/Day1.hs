module Day1 where

import Data.Maybe
import Data.Tuple.Extra
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = T.interact (T.pack . show . solve)

solve :: Text -> (Int, Int)
solve = p1 &&& p2 . T.unpack

p1 :: Text -> Int
p1 = T.foldr selector 0
-- p1 = uncurry (-) . both T.length . T.partition ('('==). T.dropWhileEnd isSpace

p2 :: String -> Int
p2 = length . takeWhile (>=0) . scanl (flip selector) 0
-- p2 :: String -> Int
-- p2 = fromLeft maxBound . bimapBoth (snd . fst) . mapAccumM goM (0,0)
--   goM :: (Int, Int) -> Char -> Either ((Int, Int), Char) ((Int, Int), Char)
--   goM (acc, iter) elem@'(' = Right ((succ acc, succ iter), elem)
--   goM (x@0, iter) elem@')' = Left  ((pred   x, succ iter), elem)
--   goM (acc, iter) elem@')' = Right ((pred acc, succ iter), elem)

selector :: Enum a => Char -> a -> a
selector = fromMaybe id . flip lookup [('(', succ), (')', pred)]
