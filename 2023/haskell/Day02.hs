module Day where

import Data.List.Extra

solve = sumOn' fst . filter snd . zip [1..] . map (and . snd . mapAccumL go (12,13,14) . concatMap (map (splitOn " " . trim) . splitOn ",") . splitOn ";" . drop 2 . dropWhile (':'/=)) . lines <$> readFile "in/d2"

go (red, green, blue) [num, "red"  ] = let x = red   - read num in let y = x >= 0 in ((x,   green, blue), y)
go (red, green, blue) [num, "green"] = let x = green - read num in let y = x >= 0 in ((red, x,     blue), y)
go (red, green, blue) [num, "blue" ] = let x = blue  - read num in let y = x >= 0 in ((red, green, x   ), y)

