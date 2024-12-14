module Day03 where

import Data.Char
import Data.Maybe
import Text.ParserCombinators.ReadP
import Data.List.Extra
import Data.Tuple.Extra

main :: IO ()
main = interact (show . solve . parse)

solve :: [Instr] -> (Int, Int)
solve = p1 &&& p2

p1, p2 :: [Instr] -> Int
p1 = sumOn' getVal where
  getVal (Val x) = x
  getVal _       = 0
p2 = snd . foldl go (True, 0) where
  go (b    , acc) Enable  = (True , acc    )
  go (b    , acc) Disable = (False, acc    )
  go (False, acc) (Val x) = (False, acc    )
  go (True , acc) (Val x) = (True , acc + x)

parse :: String -> [Instr]
parse = fst . last . readP_to_S instrSubseq

-------------
--- Utils ---
-------------
data Instr = Enable | Disable | Val Int deriving (Show, Eq)

instrSubseq :: ReadP [Instr]
instrSubseq = subseq $ choice [mul, enable, disable]

subseq :: ReadP a -> ReadP [a]
subseq p = catMaybes <$> many (attempt p)

attempt :: ReadP a -> ReadP (Maybe a)
attempt p = (Just <$> p) <++ (Nothing <$ get)

mul, enable, disable :: ReadP Instr
mul     = Val     <$> (string "mul" *> parens (around (*) nat comma))
enable  = Enable  <$   string "do()"
disable = Disable <$   string "don't()"

nat :: ReadP Int
nat = read <$> munch1 isDigit

parens :: ReadP a -> ReadP a
parens = between (char '(') (char ')')

around :: (a -> a -> c) -> ReadP a -> ReadP b -> ReadP c
around bin p sep = bin <$> p <* sep <*> p

comma :: ReadP Char
comma = char ','
