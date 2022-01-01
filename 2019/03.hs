import Advent
import Data.List ( elemIndex )
import Data.Maybe ( mapMaybe )
import qualified Data.Set as Set

parseInput :: String -> [[Vec2]]
parseInput = map (concatMap readStep . splitOn ',') . lines
    where readStep (x:xs) = replicate (read xs) $ case x of
            'U' -> ( 0, 1)
            'D' -> ( 0,-1)
            'L' -> (-1, 0)
            'R' -> ( 1, 0)

positions :: [Vec2] -> [Vec2]
positions = scanl vec2Add (0,0)

intersections :: [[Vec2]] -> [Vec2]
intersections = Set.toList . foldr1 Set.intersection . map Set.fromList

delay :: Vec2 -> [[Vec2]] -> Int
delay position = sum . mapMaybe (fmap (+1) . elemIndex position)

partOne = minimum . map (manhattan (0,0)) . intersections . map (tail . positions)
partTwo input = minimum $ map (`delay` pos) $ intersections $ pos
    where pos = map (tail . positions) input

main = runSolutions [partOne, partTwo] parseInput
