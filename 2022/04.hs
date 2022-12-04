import Advent
import Data.List

parse :: String -> [[(Int, Int)]]
parse = map (map (toPair . map read . splitOn '-') . splitOn ',') . lines
    where toPair [x,y] = (x,y)

rangeToList :: (Int, Int) -> [Int]
rangeToList (x,y) = [x..y]

contains :: Eq a => [a] -> [a] -> Bool
contains xs ys = union ys xs == ys

fullOverlap :: Eq a => [[a]] -> Bool
fullOverlap xs = any (contains everything) xs
    where everything = foldr1 union xs

anyOverlap :: Eq a => [[a]] -> Bool
anyOverlap xs = not (null (foldr1 intersect xs))

partOne = length . filter fullOverlap . map (map rangeToList)
partTwo = length . filter anyOverlap . map (map rangeToList)

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
