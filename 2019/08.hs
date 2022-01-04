import Advent
import Data.Char ( digitToInt, isDigit )
import Data.List ( minimumBy )
import Data.Ord ( comparing )

parseInput :: String -> [Int]
parseInput = map digitToInt . filter isDigit

height =  6
width  = 25

black       = 0
white       = 1
transparent = 2

-- Split a list into segments of a given length
segment :: Int -> [a] -> [[a]]
segment n xs = case splitAt n xs of
    (curr, [])   -> [curr]
    (curr, rest) -> curr : segment n rest

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

layers :: [Int] -> [[Int]]
layers = segment (height * width)

minimumWith :: Ord b => (a -> b) -> [a] -> a
minimumWith = minimumBy . comparing

applyLayer :: Int -> Int -> Int
applyLayer over under = if over == transparent then under else over

plot :: [Int] -> String
plot image = unlines
    [ [ if image !! (r * width + c) == white then '#' else ' '
    | c <- [0..width-1] ]
    | r <- [0..height-1] ]

partOne = show . product . sequence [count 1, count 2] . minimumWith (count 0) . layers
partTwo = plot . foldr1 (zipWith applyLayer) . layers

main = runSolutionsStr [partOne, partTwo] parseInput
