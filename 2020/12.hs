import Control.Arrow ( first, second )

data Mode = Position | Heading
data Instruction = Translate Vector | Rotate Int | Forward Int
type Vector = (Int,Int)
-- A ship has a heading ("waypoint") and a position
type Ship = (Vector,Vector)

parse :: String -> [Instruction]
parse = map readInstruction . lines

readInstruction :: String -> Instruction
readInstruction (action:number) =
    let i = read number in
    case action of
        'N' -> Translate ( 0,  i)
        'S' -> Translate ( 0, -i)
        'E' -> Translate ( i,  0)
        'W' -> Translate (-i,  0)
        'L' -> Rotate    (-(div i 90))
        'R' -> Rotate      (div i 90)
        'F' -> Forward   i

addVec :: Vector -> Vector -> Vector
addVec (x1,y1) (x2,y2) = (x1+x2, y1+y2)

-- Rotate a vector in 90-degree steps right (+) or left (-)
rotate :: Int -> Vector -> Vector
rotate r vec = iterate rot vec !! abs r
    where direction = signum r
          rot (x,y) = (direction * y, direction * (-x))

step :: Mode -> Instruction -> Ship -> Ship
step Position (Translate t) = second (addVec t)
step Heading  (Translate t) = first  (addVec t)
step _        (Rotate r)    = first  (rotate r)
step _        (Forward f)   = forward f
    where forward f ((x,y),pos) = ((x, y), addVec (f*x, f*y) pos)

-- Calculate the manhattan distance the ship will be from its starting
-- location after following the instructions. The mode indicates whether
-- the position or the heading of the ship is changed in response to
-- north/south/east/west instructions.
solve :: Mode -> Ship -> [Instruction] -> Int
solve mode ship = manhattanDistance . snd . foldl (flip (step mode)) ship
    where manhattanDistance (x,y) = abs x + abs y

partOne = solve Position ((1,0), (0,0))
partTwo = solve Heading ((10,1), (0,0))

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
