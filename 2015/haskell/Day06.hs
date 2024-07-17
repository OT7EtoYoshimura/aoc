module Day06 where

import Control.Applicative
import Data.Char
import Data.Maybe
import Data.Composition
import Text.ParserCombinators.ReadP

newtype Coords = Coords (Int, Int) deriving (Show, Eq)
data Dir = On     Coords Coords
         | Off    Coords Coords
         | Toggle Coords Coords deriving (Show, Eq)

parse :: String -> [Dir]
parse = concat . map fst . (readP_to_S $ many1 $ dir <* skipSpaces)

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
