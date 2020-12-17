import Data.List

type Layout = [Coordinate]
-- (x,y,z,w)
type Coordinate = (Int,Int,Int,Int)
type Adjacency = Coordinate -> [Coordinate]

parse :: String -> Layout
parse input = concat $ zipWith (\xs y -> [(x,y,0,0) | x <- xs]) xCoords [0..]
    where xCoords = map (elemIndices '#') (lines input)

relevantCoords :: Adjacency -> Layout -> Layout
relevantCoords adjacency = nub . concatMap (\x -> x : adjacency x)

adjacentCoords3 :: Adjacency
adjacentCoords3 (x,y,z,w) = delete (x,y,z,w) [
    (x',y',z',w) |
    x' <- [x-1..x+1],
    y' <- [y-1..y+1],
    z' <- [z-1..z+1]
    ]

adjacentCoords4 :: Adjacency
adjacentCoords4 (x,y,z,w) = delete (x,y,z,w) [
    (x',y',z',w') |
    x' <- [x-1..x+1],
    y' <- [y-1..y+1],
    z' <- [z-1..z+1],
    w' <- [w-1..w+1]
    ]

adjacent :: Adjacency -> Layout -> Coordinate -> [Coordinate]
adjacent adjacency layout = intersect layout . adjacency

isAlive :: Adjacency -> Layout -> Coordinate -> Bool
isAlive adjacency layout coord = if wasAlive
                                 then elem numNeighbors [2,3]
                                 else numNeighbors == 3
    where wasAlive     = elem coord layout
          numNeighbors = length (adjacent adjacency layout coord)

step :: Adjacency -> Layout -> Layout
step adjacency layout = filter (isAlive adjacency layout) (relevantCoords adjacency layout)

partOne = length . (!! 6) . iterate (step adjacentCoords3)
partTwo = length . (!! 6) . iterate (step adjacentCoords4)

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
