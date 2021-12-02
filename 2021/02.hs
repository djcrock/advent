type Vec2 = (Int, Int)
type Vec3 = (Int, Int, Int)

parse :: String -> [[String]]
parse = map words . lines

inputToVector :: [String] -> Vec2
inputToVector [direction, magnitude]
    | direction == "forward" = (mag, 0)
    | direction == "down"    = (0, mag)
    | direction == "up"      = (0, -mag)
    | otherwise              = (0, 0)
    where mag = read magnitude

swim :: Vec3 -> [Vec2] -> Vec3
swim position []               = position
swim (x,y,aim) ((dx,dy):moves) = swim (x+dx, y+dy, aim) moves

swimAim :: Vec3 -> [Vec2] -> Vec3
swimAim position []                 = position
swimAim (x,y,aim) ((dx,dAim):moves) = swimAim (x+dx, y+(dx*aim), aim+dAim) moves

toSolution (x,y,_) = x*y

zeroPosition = (0, 0, 0)

partOne = toSolution . swim zeroPosition . map inputToVector
partTwo = toSolution . swimAim zeroPosition . map inputToVector

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
