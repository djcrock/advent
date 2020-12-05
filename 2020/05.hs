import Data.Bits
import Data.List

type BoardingPass = [Bool]
type Seat = Int

parse :: String -> [BoardingPass]
parse = map (map (`elem` "BR")) . lines

binaryToInt :: [Bool] -> Int
binaryToInt = foldl (\acc bit -> shift acc 1 + fromEnum bit) 0

getRow :: BoardingPass -> Int
getRow = binaryToInt . take 7

getColumn :: BoardingPass -> Int
getColumn = binaryToInt . drop 7

toSeat :: BoardingPass -> Seat
toSeat bp = getRow bp * 8 + getColumn bp

findEmptySeat :: [Seat] -> Seat
findEmptySeat seats = findGap (sort seats)
    where findGap (x:y:ys) =
            if (y-x) == 2
            then x + 1
            else findGap (y:ys)

partOne = maximum . map toSeat
partTwo = findEmptySeat . map toSeat

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
