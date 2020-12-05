import Data.List

type BoardingPass = [Bool]
type Seat = Int

-- 'B' and 'R' are ones, 'F' and 'L' are zeroes.
parse :: String -> [BoardingPass]
parse = map (map (`elem` "BR")) . lines

-- Boarding passes are just a binary encoding of unsigned integers.
binaryToInt :: BoardingPass -> Seat
binaryToInt = foldl (\int bit -> int * 2 + fromEnum bit) 0

-- Find the first valid seat ID not present in the input.
-- Always returns an ID greater than the minimum input ID.
findEmptySeat :: [Seat] -> Seat
findEmptySeat seats = head $ [(minimum seats)..] \\ seats

partOne = maximum . map binaryToInt
partTwo = findEmptySeat . map binaryToInt

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
