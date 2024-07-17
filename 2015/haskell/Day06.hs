module Day06 where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Composition
import Data.Function
import Data.Maybe
import Data.Tuple.Extra
import Text.ParserCombinators.ReadP

newtype Coords = Coords { unCoords :: (Int, Int) } deriving (Show, Eq)
data Dir = On     Coords Coords
         | Off    Coords Coords
         | Toggle Coords Coords deriving (Show, Eq)

parse :: String -> [Dir]
parse = concat . map fst . (readP_to_S $ many1 $ dir <* skipSpaces)

--- Grid logic utilities ---
cntCells :: Coords -> Coords -> Int
cntCells = uncurry (on (*) (uncurry range))
         . zipPair .: curry (both unCoords)

range :: Int -> Int -> Int
range = succ . abs .: (-)

-- zipPair :: ((a, b), (c, d)) -> ((a, c), (b, d))
zipPair = on (&&&) (join (***)) fst snd
-- zipPair = join (***) fst &&& join (***) snd
-- zipPair = (fst *** fst) &&& (snd *** snd)
-- zipPair ((x1, y1), (x2, y2)) = ((x1, x2), (y1, y2))
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
