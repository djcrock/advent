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

step :: (Point, [Point]) -> Char -> (Point, [Point])
step (h, ts) dir = (h', tail $ scanl follow h' ts)
    where h' = move dir h

solve :: Int -> [Char] -> Int
solve n = length . nub . map (last . snd) . scanl step ((0,0),replicate n (0,0))

partOne = solve 1
partTwo = solve 9

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
