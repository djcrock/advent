import Data.List

type Data = [Int]
type Chunk = [Int]

lookback = 25

parse :: String -> [Int]
parse = map read . lines

-- Copied from 01.hs :D
chooseTwo :: [a] -> [[a]]
chooseTwo []     = []
chooseTwo (x:xs) = map (:[x]) xs ++ chooseTwo xs

-- Break the data into overlapping chunks
-- The init of each chunk is the preamble
-- The last of each chunk is the item being validated
chunks :: Data -> [Chunk]
chunks = takeWhile ((== lookback + 1) . length)
       . map (take (lookback + 1))
       . tails

validate :: Chunk -> Bool
validate chunk = any ((== value) . sum) (chooseTwo preamble)
    where preamble = init chunk
          value    = last chunk

findFirstInvalidNumber :: Data -> Int
findFirstInvalidNumber = last . head . filter (not . validate) . chunks

-- Take elements from the given list until their sum satisfies the predicate
takeWhileSum :: (Int -> Bool) -> [Int] -> [Int]
takeWhileSum pred xs = take initLength xs
    where initLength = length $ takeWhile pred (tail $ scanl (+) 0 xs)

-- Find a contigouous set of at least two numbers that sum to the given number
findWeakRange :: Data -> Int -> Chunk
findWeakRange [] _    = []
findWeakRange dat num = if sum candidateRange == num
                        then candidateRange
                        else findWeakRange (tail dat) num
    where candidateRange = takeWhileSum (<= num) dat

findWeakness :: Data -> Int
findWeakness dat = minimum weakRange + maximum weakRange
    where invalid        = findFirstInvalidNumber dat
          weakRange      = findWeakRange dat invalid

partOne = findFirstInvalidNumber
partTwo = findWeakness

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
