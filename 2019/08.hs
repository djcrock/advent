import Advent
import Data.Char ( digitToInt, isDigit )

parseInput :: String -> [Int]
parseInput = map digitToInt . filter isDigit

height =  6
width  = 25

black       = 0
white       = 1
transparent = 2

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

layers :: [Int] -> [[Int]]
layers = segment (height * width)

applyLayer :: Int -> Int -> Int
applyLayer over under = if over == transparent then under else over

plot :: [Int] -> String
plot image = unlines
    [ [ if image !! (r * width + c) == white then '#' else ' '
    | c <- [0..width-1] ]
    | r <- [0..height-1] ]

partOne = show . product . sequence [count 1, count 2] . minimumOn (count 0) . layers
partTwo = plot . foldr1 (zipWith applyLayer) . layers

main = runSolutionsStr [partOne, partTwo] parseInput
