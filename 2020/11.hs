import Data.List ( delete, find )
import Data.Maybe ( catMaybes, fromMaybe, mapMaybe )

data Position = Empty | Occupied | Floor deriving Eq
instance Show Position where
    show Empty    = "L"
    show Occupied = "#"
    show Floor    = "."

charToPosition :: Char -> Position
charToPosition 'L' = Empty
charToPosition '#' = Occupied
charToPosition _   = Floor

type Layout = [[Position]]
type Coordinate = (Int,Int)
type NeighborFinder = Layout -> Coordinate -> [Position]

parse :: String -> Layout
parse = map (map charToPosition) . lines

showLayout :: Layout -> String
showLayout = unlines . map (concatMap show)

getMaybe :: Int -> [a] -> Maybe a
getMaybe x ys = if x < 0 || x >= length ys
                then Nothing
                else Just (ys !! x)

getPosition :: Layout -> Coordinate -> Maybe Position
getPosition layout (x,y) = getMaybe y layout >>= getMaybe x

adjacentCoords :: Coordinate -> [Coordinate]
adjacentCoords (x,y) = delete (x,y) [(x',y') | x' <- [x-1..x+1], y' <- [y-1..y+1]]

adjacent :: NeighborFinder
adjacent layout coord = map (fromMaybe Floor . getPosition layout) (adjacentCoords coord)

-- Given a position and those adjacent to it, determine the next position
stepPosition :: Int -> Position -> [Position] -> Position
stepPosition _ Floor _                 = Floor
stepPosition _ Empty adj               = if notElem Occupied adj
                                         then Occupied
                                         else Empty
stepPosition maxNeighbors Occupied adj = if length (filter (== Occupied) adj) >= maxNeighbors
                                         then Empty
                                         else Occupied

stepLayout :: NeighborFinder -> Int -> Layout -> Layout
stepLayout neighborFinder maxNeighbors layout = map stepRow [0..height-1]
    where width  = length (head layout)
          height = length layout
          stepRow y = map (\x -> stepCell (x,y)) [0..width-1]
          stepCell coord = stepPosition
                                maxNeighbors
                                (fromMaybe Floor $ getPosition layout coord)
                                (neighborFinder layout coord)

-- Given a list of simulated layout states, get the stable state
findStoppingPoint :: [Layout] -> Layout
findStoppingPoint (x:y:ys) = if x == y then x else findStoppingPoint (y:ys)

countInstances :: Position -> Layout -> Int
countInstances pos = length . filter (== pos) . concat

-- Given a starting coordinate and a direction, trace the line of sight
lineOfSight :: Coordinate -> Coordinate -> [Coordinate]
lineOfSight (x,y) (dx,dy) = next : lineOfSight next (dx,dy)
    where next = (x+dx,y+dy)

visible :: NeighborFinder
visible layout coord = catMaybes firstVisible
    where directions = adjacentCoords (0,0)
          linesOfSight = map (map (getPosition layout) . lineOfSight coord) directions
          firstVisible = mapMaybe (find (`elem` [Just Occupied, Just Empty, Nothing])) linesOfSight

partOne = countInstances Occupied . findStoppingPoint . iterate (stepLayout adjacent 4)
partTwo = countInstances Occupied . findStoppingPoint . iterate (stepLayout visible 5)

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
