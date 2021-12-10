import Data.Either
import Data.List ( sort )

isOpen :: Char -> Bool
isOpen = (`elem` "([{<")

isClose = not . isOpen

match :: Char -> Char
match '(' = ')'
match '[' = ']'
match '{' = '}'
match '<' = '>'
match _   = error "Invalid opening character"

scoreIllegalChar :: Char -> Int
scoreIllegalChar ')' = 3
scoreIllegalChar ']' = 57
scoreIllegalChar '}' = 1197
scoreIllegalChar '>' = 25137

scoreCompletionChar :: Char -> Int
scoreCompletionChar ')' = 1
scoreCompletionChar ']' = 2
scoreCompletionChar '}' = 3
scoreCompletionChar '>' = 4

isMatch :: Char -> Char -> Bool
isMatch c1 c2 = match c1 == c2

-- Left = corrupted character, Right = incomplete stack
process :: String -> String -> Either Char String
process xs [] = Right xs
process [] (y:ys)
    | isOpen y    = process [y] ys
    | otherwise   = Left y
process (x:xs) (y:ys)
    | isOpen y    = process (y:x:xs) ys
    | isMatch x y = process xs ys
    | otherwise   = Left y

complete :: String -> String
complete = map match

scoreCompletion :: String -> Int
scoreCompletion = foldl (\acc item -> (acc * 5) + scoreCompletionChar item) 0

median :: [Int] -> Int
median xs = (sort xs) !! div (length xs) 2

partOne = sum . map scoreIllegalChar . lefts . map (process "")
partTwo = median . map (scoreCompletion . complete) . rights . map (process "")

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . lines
