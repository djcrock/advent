import Data.Char ( digitToInt )
import qualified Data.Map as Map
import qualified Data.Set as Set

type Grid = [[Int]]
type Coordinate = (Int, Int)
type Distances = Map.Map Coordinate Int
type PQueue = Set.Set (Int, Coordinate)
type Unvisited = Set.Set Coordinate
type State = (Grid, Distances, PQueue, Unvisited)

parse :: String -> Grid
parse = map (map digitToInt) . lines

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
visit :: Coordinate -> State -> State
visit current (g, dists, pq, unvis) =
    foldr update (g, dists, pq, Set.delete current unvis) adj
    where newDist       = (+ (dists Map.! current)) . weight
          adj           = filter (`Set.member` unvis) (adjacent g current)
          update coord  = markDistance coord (newDist coord)
          weight (r, c) = g !! r !! c

search :: State -> Coordinate -> Coordinate -> Distances
search state@(_,dists,_,_) current dest
    | current == dest = dists
    | otherwise       = search state' next dest
    where (state', next) = nextVisit (visit current state)

dijkstra :: Grid -> Int
dijkstra g = result Map.! dest
    where dest   = ((length g)-1, (length $ head g)-1)
          coords = coordinates g
          dists  = Map.insert (0,0) 0 $ Map.fromList $ map (,maxBound) coords
          unvis  = Set.fromList $ coords
          pq     = Set.fromList $ map (maxBound,) coords
          result = search (g, dists, pq, unvis) (0,0) dest

bigger :: Int -> Grid -> Grid
bigger factor = longer . map wider
    where inc n  = (mod n 9) + 1
          wider  = concat . take factor . iterate (map inc)
          longer = concat . take factor . iterate (map $ map inc)

partOne = dijkstra
partTwo = dijkstra . bigger 5

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
