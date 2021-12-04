import Data.List ( transpose )

parse :: String -> [[Bool]]
parse = map (map (=='1')) . lines

binaryToInt :: [Bool] -> Int
binaryToInt = foldl (\int bit -> int * 2 + fromEnum bit) 0

mostCommon :: [Bool] -> Bool
mostCommon xs = trueCount >= halfLength
    where trueCount  = sum (map fromEnum xs)
          halfLength = (div (length xs) 2) + (mod (length xs) 2)

gamma :: [[Bool]] -> Int
gamma = binaryToInt . map mostCommon . transpose

epsilon :: [[Bool]] -> Int
epsilon = binaryToInt . map (not . mostCommon) . transpose

filterByPositions :: Int -> ([Bool] -> Bool) -> [[Bool]] -> [Bool]
filterByPositions _ _ [x] = x
filterByPositions pos pred xs = filterByPositions (pos + 1) pred filtered
    where bitMustBe = pred $ (transpose xs) !! pos
          filtered  = filter (\ys -> (ys !! pos) == bitMustBe) xs

oxygenRating :: [[Bool]] -> Int
oxygenRating = binaryToInt . filterByPositions 0 mostCommon

co2Rating :: [[Bool]] -> Int
co2Rating = binaryToInt . filterByPositions 0 (not . mostCommon)

partOne = product . sequence [gamma, epsilon]
partTwo = product . sequence [oxygenRating, co2Rating]

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
