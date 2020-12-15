import Data.List ( elemIndex )

parse :: String -> [Int]
parse = map read . lines

-- Given a target position n and a stack of previous numbers,
-- determine what the nth number spoken will be.
countTo :: Int -> [Int] -> Int
countTo n (x:xs) = if length (x:xs) == n then x else countTo n (next:x:xs)
    where next = case elemIndex x xs of
                   Nothing -> 0
                   Just y  -> y + 1

partOne = countTo 2020 . reverse
partTwo = partOne

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
