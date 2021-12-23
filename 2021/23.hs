import Data.List ( (\\), delete, elemIndex, intersect, nub )
import Data.Maybe ( mapMaybe )
import qualified Data.Map as Map
import qualified Data.Set as Set

type Coord = (Int,Int)
type Burrow = Map.Map Coord Char
type Distances = Map.Map Burrow (Int,[Burrow])
type PQueue = Set.Set (Int, Burrow)
type Unvisited = Set.Set Burrow
type State = (Int, Distances, PQueue, Unvisited)

hallway      = zip [0..10] [0,0..]
rooms depth  = [(x,y) | x <- [2,4,6,8], y <- [1..depth]]
illegalStops = zip [2,4,6,8] [0,0..]
hallwayStops = hallway \\ illegalStops

start1 :: Burrow
start1 = Map.fromList $ zip (rooms 2) "BCDDCBAA"

start2 :: Burrow
start2 = Map.fromList $ zip (rooms 4) "BDDCDCBDCBABAACA"

finish :: Int -> Burrow
finish n = Map.fromList $ zip (rooms n) $ concatMap (replicate n) "ABCD"

cost :: Char -> Int
cost c = maybe 0 (10^) (elemIndex c "ABCD")

room :: Char -> Int
room c = maybe 0 ((+2) . (*2)) (elemIndex c "ABCD")

path :: Coord -> Coord -> [Coord]
path from@(fx,fy) to@(tx,ty)
    | from == to         = []
    | fx /= tx && fy > 0 = (fx,fy-1)    : path (fx,fy-1)    to
    | fx /= tx           = (fx+xDir,fy) : path (fx+xDir,fy) to
    | fx == tx           = (fx,fy+yDir) : path (fx,fy+yDir) to
    where xDir = signum (tx-fx)
          yDir = signum (ty-fy)

legalPaths :: Int -> Burrow -> Coord -> [[Coord]]
legalPaths depth burrow from = filter unoccupied $ map (path from) targets
    where unoccupied = null . intersect (Map.keys burrow)
          targets    = legalTargets depth burrow from

legalTargets :: Int -> Burrow -> Coord -> [Coord]
legalTargets depth burrow from
    | not isInHall  &&     roomHasOther = hallwayStops
    | isInHall      && not roomHasOther = [bestRoomCoord]
    | isInOtherRoom && not roomHasOther = bestRoomCoord : hallwayStops
    | otherwise                         = []
    where creature      = burrow Map.! from
          roomCoords    = map (room creature,) [1..depth]
          bestRoomCoord = last $ roomCoords \\ (Map.keys burrow)
          roomContents  = mapMaybe (`Map.lookup` burrow) roomCoords
          roomHasOther  = not $ null $ delete creature (nub roomContents)
          isInRoom      = elem from roomCoords
          isInHall      = elem from hallway
          isInOtherRoom = (not isInRoom) && (not isInHall)

moves :: Int -> Burrow -> [(Int,Burrow)]
moves depth burrow = concatMap (creatureMoves depth burrow) (Map.keys burrow)

creatureMoves :: Int -> Burrow -> Coord -> [(Int,Burrow)]
creatureMoves depth burrow c =
    map (\p -> (pathCost p, moveCreature p)) creaturePaths
    where creature = burrow Map.! c
          creaturePaths = legalPaths depth burrow c
          pathCost p = cost creature * length p
          moveCreature p = Map.insert (last p) creature $ Map.delete c burrow

nextVisit :: State -> (State, Burrow)
nextVisit (depth, dists, pq, vis) = ((depth, dists, rest, vis), c)
    where ((_,c), rest) = Set.deleteFindMin pq

markDistance :: Burrow -> Int -> [Burrow] -> State -> State
markDistance burrow distance path state@(depth, dists, pq, vis) =
    if distance < maybe maxBound fst (Map.lookup burrow dists) then
        ( depth
        , Map.insert burrow (distance,path) dists
        , Set.insert (distance, burrow) pq
        , vis )
        else state

visit :: Burrow -> State -> State
visit current (depth, dists, pq, vis) =
    foldr update (depth, dists, pq, Set.insert current vis) adj
    where (dist,path)  = dists Map.! current
          adj          = filter ((`Set.notMember` vis) . snd) (moves depth current)
          update (w,b) = markDistance b (w + dist) (b:path)

search :: State -> Burrow -> Burrow -> Distances
search state@(_,dists,_,_) current dest
    | current == dest = dists
    | otherwise       = search state' next dest
    where (state', next) = nextVisit (visit current state)

-- Grabbed this from my day 13 solution :)
dijkstra :: Burrow -> Int -> (Int,[Burrow])
dijkstra start depth = result Map.! (finish depth)
    where dists  = Map.singleton start (0,[start])
          vis    = Set.empty
          pq     = Set.empty
          result = search (depth, dists, pq, vis) start (finish depth)

plotResult :: Int -> (Int,[Burrow]) -> String
plotResult depth (c,bs) = unlines $ (show c) : map plotBurrow (reverse bs)
    where plotBurrow b = unlines
            [ [ maybe (emptySpace (x,y)) id (Map.lookup (x,y) b)
            | x <- [0..(length hallway)-1] ]
            | y <- [0..depth] ]
          emptySpace c = if elem c (hallway ++ rooms depth) then '.' else ' '

partOne = plotResult 2 $ dijkstra start1 2
partTwo = plotResult 4 $ dijkstra start2 4

main = interact $ unlines . pure [partOne, partTwo]
