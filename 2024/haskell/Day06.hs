module Day06 where

import Data.List.Extra
import Data.Tuple.Extra
import Data.Maybe
import Control.Applicative
import Data.Composition

type Coords = (Int, Int) -- (x, y)
data Dir = U | D | R | L deriving (Eq, Show)

main :: IO ()
main = interact (show . solve . parse)

parse :: String -> ([Coords], Int, (Dir, Coords))
parse = uncurry3 (liftA3 (,,)) (parseObstacles, pred . length, parseGuard) . lines

solve :: ([Coords], Int, (Dir, Coords)) -> (Int, Int)
solve = p1 &&& p2

p1, p2 :: ([Coords], Int, (Dir, Coords)) -> Int
p1 = length . nubOrd . (uncurry3 $ unfoldr .: move)
p2 = const maxBound

-------------
--- Utils ---
-------------
parseObstacles :: [String] -> [Coords]
parseObstacles = concat . zipWithFrom (map . flip (,)) 0 . map (elemIndices '#')

parseGuard :: [String] -> (Dir, Coords)
parseGuard = (U,) . head . catMaybes . zipWith (liftA2 $ flip (,)) (map Just [0..]) . map (elemIndex '^')

move :: [Coords]      -- List of obstacle coordinates
     -> Int           -- Max map size
     -> (Dir, Coords) -- Current guard direction and position
     -> Maybe (Coords, (Dir, Coords))
move _ border (_, (x, y)) | x < 0 || y < 0 || x > border || y > border = Nothing
move obstacles _ (U, c) = let moved = second pred c in
                            if   moved `elem` obstacles
                            then Just (c, (R, c)    )
                            else Just (c, (U, moved))
move obstacles _ (D, c) = let moved = second succ c in
                            if   moved `elem` obstacles
                            then Just (c, (L, c)    )
                            else Just (c, (D, moved))
move obstacles _ (R, c) = let moved = first succ c in
                            if   moved `elem` obstacles
                            then Just (c, (D, c)    )
                            else Just (c, (R, moved))
move obstacles _ (L, c) = let moved = first pred c in
                            if   moved `elem` obstacles
                            then Just (c, (U, c)    )
                            else Just (c, (L, moved))
