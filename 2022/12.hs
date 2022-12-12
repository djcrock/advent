import Data.List ( elemIndex, findIndex )
import Data.Maybe ( fromJust )
import qualified Data.Map as Map
import qualified Data.Set as Set

type Grid = [[Char]]
type Coordinate = (Int, Int)
type Distances = Map.Map Coordinate Int
type PQueue = Set.Set (Int, Coordinate)
type Unvisited = Set.Set Coordinate
type State = (Grid, Distances, PQueue, Unvisited)

height :: Char -> Int
height 'S' = height 'a'
height 'E' = height 'z'
height  c  = fromEnum c

get :: Grid -> Coordinate -> Char
get g (r, c) = g !! r !! c

coordinates :: Grid -> [Coordinate]
coordinates [] = []
coordinates g =
    [ (r, c)
    | r <- [0..(length g)-1]
    , c <- [0..(length $ head g)-1] ]

adjacent :: Grid -> Coordinate -> [Coordinate]
adjacent grid (r, c) =
    [ (r', c')
    | (r', c') <- [(r-1, c), (r, c+1), (r+1, c), (r, c-1)]
    , r' >= 0 && r' < length grid
    , c' >= 0 && c' < length (head grid) ]

adjacentUphill :: Grid -> Coordinate -> [Coordinate]
adjacentUphill g (r, c) = filter ((<=(1 + height (get g (r, c)))) . height . get g) adj
    where adj = adjacent g (r, c)

adjacentDownhill :: Grid -> Coordinate -> [Coordinate]
adjacentDownhill g (r, c) = filter ((>=((height (get g (r, c))) - 1)) . height . get g) adj
    where adj = adjacent g (r, c)

-- Pop a value off of the priority queue
-- It might be possible to get an already visited coordinate
-- But I haven't encountered any issues
nextVisit :: State -> (State, Coordinate)
nextVisit (g, dists, pq, unvis) = ((g, dists, rest, unvis), c)
    where ((_,c), rest) = Set.deleteFindMin pq

-- Update the given coordinate's distance if it is lower than before
markDistance :: Coordinate -> Int -> State -> State
markDistance coord distance state@(g, dists, pq, unvis) =
    if distance < (dists Map.! coord) then
        ( g
        , Map.insert coord distance dists
        , Set.insert (distance, coord) pq
        , unvis )
        else state

-- Visit a coordinate, updating distances for adjacent coords
visit :: Coordinate -> State -> (Coordinate -> [Coordinate]) -> State
visit current (g, dists, pq, unvis) getAdjacent =
    foldr update (g, dists, pq, Set.delete current unvis) adj
    where newDist       = (+ (dists Map.! current)) . weight
          adj           = filter (`Set.member` unvis) (getAdjacent current)
          update coord  = markDistance coord (newDist coord)
          weight (r, c) = 1

search :: State -> Coordinate -> (Coordinate -> Bool) -> (Coordinate -> [Coordinate] )-> Int
search state@(_,dists,_,_) current isDest getAdjacent
    | isDest current = dists Map.! current
    | otherwise      = search state' next isDest getAdjacent
    where (state', next) = nextVisit (visit current state getAdjacent)

findCoord :: Char -> Grid -> Coordinate
findCoord c g = (row, col)
    where row = fromJust $ findIndex (elem c) g
          col = fromJust $ elemIndex c (g !! row)

dijkstra :: Grid -> Coordinate -> (Coordinate -> Bool) -> (Coordinate -> [Coordinate]) -> Int
dijkstra g start isDest getAdjacent = result
    where dest   = findCoord 'E' g
          coords = coordinates g
          dists  = Map.insert start 0 $ Map.fromList $ map (,maxBound) coords
          unvis  = Set.fromList $ coords
          pq     = Set.fromList $ map (maxBound,) coords
          result = search (g, dists, pq, unvis) start isDest getAdjacent

partOne grid = dijkstra grid
    (findCoord 'S' grid)
    ((=='E') . get grid)
    (adjacentUphill grid)
partTwo grid = dijkstra grid
    (findCoord 'E' grid)
    ((== height 'a') . height . get grid)
    (adjacentDownhill grid)

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . lines
