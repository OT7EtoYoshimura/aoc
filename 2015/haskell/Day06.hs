module Day06 where

import Control.Applicative
import Data.Char
import Data.Maybe
import Control.Monad
import Data.Composition
import Data.Tuple.Extra
import Data.Function
import Text.ParserCombinators.ReadP

newtype Coords = Coords { unCoords :: (Int, Int) } deriving (Show, Eq)
data Dir = On     Coords Coords
         | Off    Coords Coords
         | Toggle Coords Coords deriving (Show, Eq)

--- Standard boilerplatea ---
main :: IO ()
main = interact (show . solve . parse)

parse :: String -> [Dir]
parse = concatMap fst . readP_to_S (many1 $ dir <* skipSpaces)

solve :: [Dir] -> (Int, Int)
solve = p1 &&& p2

p1, p2 :: [Dir] -> Int
p1 = error "unimplemented: p1"
p2 = error "unimplemented: p2"
-----------------------------

--- Grid logic utilities ---
cntCells :: Coords -> Coords -> Int
cntCells = uncurry (on (*) (uncurry range))
         . zipPair .: curry (both unCoords)

range :: Int -> Int -> Int
range = succ . abs .: (-)

-- zipPair :: ((a, b), (c, d)) -> ((a, c), (b, d))
zipPair = on (&&&) (join (***)) fst snd
-----------------------

--- ReadP parsing utilities ---
dir :: ReadP Dir
dir = makeDir <$> command <* skipSpaces <*> coords <* skipSpaces <* string "through" <* skipSpaces <*> coords

makeDir :: String -> Coords -> Coords -> Dir
makeDir = fromMaybe (error "makeDir") . flip lookup [ ("turn on" , On    )
                                                    , ("turn off", Off   )
                                                    , ("toggle"  , Toggle) ]

command :: ReadP String
command = choice $ string <$> ["turn on", "turn off", "toggle"]

coords :: ReadP Coords
coords = Coords <$> surround int (char ',')

int :: ReadP Int
int = read <$> munch1 isDigit

surround :: ReadP a -> ReadP b -> ReadP (a, a)
surround p sep = (,) <$> p <* sep <*> p
-------------------------------
