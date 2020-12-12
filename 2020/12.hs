data Action = N | S | E | W | L | R | F deriving (Eq, Read, Show)
data Direction = North | East | South | West deriving (Bounded, Enum, Eq, Show)
type Step = (Action,Int)
type Instructions = [Step]
type Delta = (Int,Int)
type Position = (Int,Int)
type ShipState = (Direction,Position)
type State = (Position,ShipState)

parse :: String -> Instructions
parse = map readLine . lines
    where readLine (act:num) = (read [act], read num)

turnLeft :: Direction -> Direction
turnLeft dir = if dir == minBound then maxBound else pred dir

turnRight :: Direction -> Direction
turnRight dir = if dir == maxBound then minBound else succ dir

move :: Delta -> Position -> Position
move (dx,dy) (x,y) = (x+dx,y+dy)

moveShip :: (Position -> Position) -> ShipState -> ShipState
moveShip delta (dir,pos) = (dir, delta pos)

moveDir :: Direction -> Int -> Position -> Position
moveDir North i = move ( 0,  i)
moveDir South i = move ( 0, -i)
moveDir East  i = move ( i,  0)
moveDir West  i = move (-i,  0)

stepShip :: Step -> ShipState -> ShipState
stepShip (N,i) ship      = moveShip (moveDir North i) ship
stepShip (S,i) ship      = moveShip (moveDir South i) ship
stepShip (E,i) ship      = moveShip (moveDir East  i) ship
stepShip (W,i) ship      = moveShip (moveDir West  i) ship
stepShip (F,i) (dir,pos) = moveShip (moveDir dir   i) (dir,pos)
stepShip (L,i) (dir,pos) = (iterate turnLeft  dir !! div i 90, pos)
stepShip (R,i) (dir,pos) = (iterate turnRight dir !! div i 90, pos)

manhattanDistance :: Position -> Int
manhattanDistance (x,y) = abs x + abs y

rotateLeft :: Position -> Position
rotateLeft (x,y) = (-y,x)

rotateRight :: Position -> Position
rotateRight (x,y) = (y,-x)

step :: Step -> State -> State
step (N,i) (waypoint,ship) = (moveDir North i waypoint, ship)
step (S,i) (waypoint,ship) = (moveDir South i waypoint, ship)
step (E,i) (waypoint,ship) = (moveDir East  i waypoint, ship)
step (W,i) (waypoint,ship) = (moveDir West  i waypoint, ship)
step (F,i) (waypoint,ship) = (waypoint, iterate (moveShip (move waypoint)) ship !! i)
step (L,i) (waypoint,ship) = (iterate rotateLeft  waypoint !! div i 90, ship)
step (R,i) (waypoint,ship) = (iterate rotateRight waypoint !! div i 90, ship)

initialShipState = (East,(0,0))
initialState = ((10,1),initialShipState)

partOne = manhattanDistance . snd . foldl (flip stepShip) initialShipState
partTwo = manhattanDistance . snd . snd . foldl (flip step) initialState

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
