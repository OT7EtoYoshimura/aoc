module Day11 where

import Data.List.Extra
import Data.Char

data Monkey = Monkey Id Items Ration Test SendTo SendTo deriving Show
data Ration = Mult Rand Rand
            | Add  Rand Rand
            deriving Show
data Rand = Old | Val Integer deriving Show

type Id = Int
type Items = [Int]
type Test = Int
type SendTo = Int

p1 :: IO [Monkey]
p1 = map parse . chunksOf 6 . filter (not . null) . lines <$> readFile "in/d11test"

parse :: [String] -> Monkey
parse [id,items,ration,test,ifTrue,ifFalse] = Monkey (parseId id) (parseItems items) (parseRation ration) (parseRest test) (parseRest ifTrue) (parseRest ifFalse)

parseId :: String -> Id
parseId = read . init . snd . word1

parseItems :: String -> Items
parseItems = map read . drop 2 . wordsBy (or . sequence [isSpace, isPunctuation])

parseRation :: String -> Ration
parseRation = parseRator . drop 3 . words

parseRator :: [String] -> Ration
parseRator [x, "*", y] = Mult (parseRand x) (parseRand y)
parseRator [x, "+", y] = Add  (parseRand x) (parseRand y)

parseRand :: String -> Rand
parseRand "old" = Old
parseRand x     = Val $ read x

parseRest :: String -> Int
parseRest = read . last . words
