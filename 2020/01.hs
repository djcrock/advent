parse :: String -> [Int]
parse = map read . lines

-- Part 1:
-- We want to check each element against all following elements to get all
-- possible pairs. Basically, generate all "choose 2" lists.
chooseTwo :: [a] -> [[a]]
chooseTwo []     = []
chooseTwo (x:xs) = map (:[x]) xs ++ chooseTwo xs

-- Part 2:
-- To generalize, let's "choose n".
choose :: Int -> [a] -> [[a]]
choose _ []     = []
choose 1 xs     = map (:[]) xs
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs

-- Does the given list sum to the given value?
sumTo x ys = x == sum ys

-- From all the possible n-combinations of the input set:
-- take those that sum to 2020
-- find their products
solve n = map product . filter (sumTo 2020) . choose n

partOne = solve 2
partTwo = solve 3

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse