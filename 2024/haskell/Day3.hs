module Day3 where

import Data.Char
import Data.Maybe
import Text.ParserCombinators.ReadP
import Data.List.Extra
import Data.Tuple.Extra

main :: IO ()
main = interact (show . solve . parse)

solve :: String -> Int
solve = p1

p1, p2 :: String -> Int
p1 = sum . fst . maximumOn (length . fst) . readP_to_S greedyMul
p2 = undefined

parse :: a -> a
parse = id

-------------
--- Utils ---
-------------
greedyMul :: ReadP [Int]
greedyMul = catMaybes <$> many (attempt mul)

attempt :: ReadP a -> ReadP (Maybe a)
attempt p = (Just <$> p) <++ (Nothing <$ get)

mul :: ReadP Int
mul = string "mul" *> parens (around (*) nat comma)

nat :: ReadP Int
nat = read <$> munch1 isDigit

parens :: ReadP a -> ReadP a
parens = between (char '(') (char ')')

around :: (a -> a -> c) -> ReadP a -> ReadP b -> ReadP c
around bin p sep = bin <$> p <* sep <*> p

comma :: ReadP Char
comma = char ','
