import Advent
import Data.Char ( digitToInt, intToDigit, isDigit )
import Data.List ( tails )

parseInput :: String -> [Integer]
parseInput = map (toInteger . digitToInt) . filter isDigit

onesDigit :: Integer -> Integer
onesDigit = flip mod 10 . abs

pattern :: Int -> [Integer]
pattern n = tail $ cycle $ concatMap (replicate n) [0,1,0,-1]

fft :: [Integer] -> [Integer]
fft xs = zipWith calcDigit (replicate (length xs) xs) patterns
    where patterns  = map pattern [1..]
          calcDigit = ((onesDigit . sum) .) . zipWith (*)

pascalDiagonal :: Integer -> [Integer]
pascalDiagonal n = 1 : diagonal 1 1
    where diagonal prev k = let curr = div (prev * (n + k)) k
                            in  curr : diagonal curr (k+1)

partOne = digitsToInt . take 8 . (!! 100) . iterate fft
-- Because of the offset and the very large input number, the "pattern" is
-- always ..0,0,1,1,1.., with the 1s starting at the digit in question.
-- A diagonal from Pascal's Triangle determines the factor by which to multiply
-- each of the digits in the sequence before summing them.
partTwo input = digitsToInt $ map calcDigit $ take 8 $ tails input'
    where offset = fromIntegral $ digitsToInt $ take 7 input
          input' = drop offset $ concat $ replicate 10000 input
          calcDigit = onesDigit . sum . zipWith (*) (pascalDiagonal 99)

main = runSolutions [partOne, partTwo] parseInput
