import Data.Char ( digitToInt, isDigit )
import Data.List ( sort )

parse :: String -> [Int]
parse input = read $ "[" ++ input ++ "]"

median :: [Int] -> Int
median nums
    | odd len   = sorted !! mid
    | otherwise = div (sum $ take 2 $ drop (mid - 1) sorted) 2
        where len    = length nums
              mid    = div len 2
              sorted = sort nums

mean :: [Int] -> Int
mean nums = div (sum nums) (length nums)

fuelCost1 :: Int -> [Int] -> Int
fuelCost1 position = sum . map (abs . ((-)position))

fuelCost2 :: Int -> [Int] -> Int
fuelCost2 position = sum . map (cost . abs . ((-)position))
    where cost distance = div (distance * (distance + 1)) 2

partOne crabs = fuelCost1 (median crabs) crabs
partTwo crabs = fuelCost2 (mean crabs) crabs

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
