import Data.List ( nub )

type Point = (Int, Int)

parse :: String -> [Char]
parse = concatMap (\(c:' ':num) -> replicate (read num) c) . lines

move :: Char -> Point -> Point
move 'U' (x, y) = (x  , y+1)
move 'D' (x, y) = (x  , y-1)
move 'R' (x, y) = (x+1, y  )
move 'L' (x, y) = (x-1, y  )

follow :: Point -> Point -> Point
follow (hx, hy) (tx, ty) = let (dx, dy) = (hx-tx, hy-ty) in
    if (abs dx) > 1 || (abs dy) > 1
        then (tx + signum dx, ty + signum dy)
        else (tx, ty)

step :: [Point] -> Char -> [Point]
step (h:ts) dir = scanl follow (move dir h) ts

solve :: Int -> [Char] -> Int
solve n = length . nub . map last . scanl step (replicate n (0,0))

partOne = solve 2
partTwo = solve 10

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
