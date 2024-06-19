module Day2 where

score1 "A X" = 4
score1 "A Y" = 8
score1 "A Z" = 3
score1 "B X" = 1
score1 "B Y" = 5
score1 "B Z" = 9
score1 "C X" = 7
score1 "C Y" = 2
score1 "C Z" = 6

score2 "A X" = 3
score2 "A Y" = 4
score2 "A Z" = 8
score2 "B X" = 1
score2 "B Y" = 5
score2 "B Z" = 9
score2 "C X" = 2
score2 "C Y" = 6
score2 "C Z" = 7

solve f = sum . map f . lines <$> readFile "d2"
p1 = solve score1
p2 = solve score2
