import Advent
import Data.Char ( chr, ord )
import Data.List ( elemIndex )
import Data.Maybe ( fromJust )

parse :: String -> [(Char, Char)]
parse = map (\(x:_:y:[]) -> (x, y)) . lines

-- Each character has a value representing its standalone score/rank.
value :: Char -> Int
value c = 1 + (fromJust $ elemIndex c "ABC")

-- Convert a value back into a character.
-- Values out of range (high or low) will "wrap" around.
fromValue :: Int -> Char
fromValue val = "ABC" !! mod (val + 2) 3

-- Char subtraction to map "XYZ" -> "ABC"
toMove :: Char -> Char
toMove c = chr $ ord c - 23

-- Based on opponent's play (ABC), satisfy the desired outcome (XYZ).
forceOutcome :: (Char, Char) -> (Char, Char)
forceOutcome (them, outcome) =
    (them, fromValue $ (value them) + (ord outcome - ord 'Y'))

score :: (Char, Char) -> Int
score (them, me) = value me + case (ord me) - (ord them) of
    -2 -> 6
    -1 -> 0
    0  -> 3
    1  -> 6
    2  -> 0

partOne = sum . map (score . fmap toMove)
partTwo = sum . map (score . forceOutcome)

main = runSolutions [partOne, partTwo] parse
