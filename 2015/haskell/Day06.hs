module Day06 where

import Control.Applicative
import Data.Char
import Data.Maybe
import Control.Monad
import Data.Composition
import Data.Tuple.Extra
import Data.Function
import Text.ParserCombinators.ReadP

newtype Coords = Coords { unCoords :: (Int   , Int   ) } deriving (Show, Eq)
newtype Range  = Range  { unRange  :: (Coords, Coords) } deriving (Show, Eq)
data Act = On     Range
         | Off    Range
         | Toggle Range deriving (Show, Eq)

--- Standard boilerplate ---
main :: IO ()
main = interact (show . solve . parse)

parse :: String -> [Act]
parse = concatMap fst . readP_to_S (many1 $ act <* skipSpaces)

solve :: [Act] -> (Int, Int)
solve = p1 &&& p2

p1, p2 :: [Act] -> Int
p1 = error "unimplemented: p1"
p2 = error "unimplemented: p2"
-----------------------------

--- Grid logic utilities ---
cntCells :: Range -> Int
cntCells = uncurry (*) . both dist . zipPair . both unCoords . unRange

dist :: (Int, Int) -> Int
dist = succ . abs . uncurry (-)

-- zipPair :: ((a, b), (c, d)) -> ((a, c), (b, d))
zipPair = on (&&&) (join (***)) fst snd
-----------------------

--- ReadP parsing utilities ---
act :: ReadP Act
act = makeAct <$> command <* skipSpaces <*> range

makeAct :: String -> Range -> Act
makeAct = fromMaybe (error "makeAct") . flip lookup [ ("turn on" , On    )
                                                    , ("turn off", Off   )
                                                    , ("toggle"  , Toggle) ]

command :: ReadP String
command = choice $ string <$> ["turn on", "turn off", "toggle"]

range :: ReadP Range
range = Range <$> surround coords (join between skipSpaces $ string "through")

coords :: ReadP Coords
coords = Coords <$> surround int (char ',')

int :: ReadP Int
int = read <$> munch1 isDigit

surround :: ReadP a -> ReadP b -> ReadP (a, a)
surround p sep = (,) <$> p <* sep <*> p
-------------------------------
