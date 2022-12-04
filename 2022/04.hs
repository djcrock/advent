parse :: String -> [((Int, Int), (Int, Int))]
parse = map (both (both read . splitOn '-') . splitOn ',') . lines
    where splitOn c xs = fmap tail $ break (==c) xs
          both f (x,y) = (f x, f y)

partOne = length . filter
    (\((x1,y1),(x2,y2)) -> (x1 >= x2 && y1 <= y2) || (x2 >= x1 && y2 <= y1))
partTwo = length . filter
    (\((x1,y1),(x2,y2)) -> (x1 <= y2 && y1 >= x2) || (x2 <= y1 && y2 >= y1))

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
