import Data.Char ( digitToInt )
import Data.List ( sort )
import qualified Data.Set as Set

type Grid = [[Int]]
type Coordinate = (Int, Int)
type Visited = Set.Set Coordinate

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

coordinates :: Grid -> [Coordinate]
coordinates [] = []
coordinates xs =
    [ (r, c)
    | r <- [0..(length xs)-1]
    , c <- [0..(length $ head xs)-1] ]

adjacent :: Grid -> Coordinate -> [Coordinate]
adjacent grid (r, c) =
    [ (r', c')
    | (r', c') <- [(r-1, c), (r, c+1), (r+1, c), (r, c-1)]
    , r' >= 0 && r' < length grid
    , c' >= 0 && c' < length (head grid) ]

getCoord :: Grid -> Coordinate -> Int
getCoord xs (r, c) = xs !! r !! c

minima :: Grid -> [Coordinate]
minima grid = filter isLocalMin (coordinates grid)
    where get = getCoord grid
          isLocalMin coord = all (> get coord) (map get $ adjacent grid coord)

-- Explore each basin, starting from its lowest point
basins :: Grid -> [[Coordinate]]
basins grid = map (Set.toList . exploreBasin grid Set.empty) $ minima grid
 
exploreBasin :: Grid -> Visited -> Coordinate -> Visited
exploreBasin grid visited coord
    | getCoord grid coord == 9 = visited
    | Set.member coord visited = visited
    | otherwise = foldr explore (Set.insert coord visited) (adjacent grid coord)
        where explore c v = exploreBasin grid v c

partOne grid = sum $ map (+1) . map (getCoord grid) $ minima grid
partTwo = product . take 3 . reverse . sort . map length . basins

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
