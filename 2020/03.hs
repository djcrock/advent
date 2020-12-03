type Grid = [[Bool]]
type Path = [Bool]
type Slope = (Int,Int)

parse :: String -> Grid
parse = map (cycle . map (=='#')) . lines

slopesToTest = [
    (1,1),
    (3,1),
    (5,1),
    (7,1),
    (1,2)
    ]

shiftGrid :: Slope -> Grid -> Grid
shiftGrid (x,y) = drop y . map (drop x)

walkPath :: Slope -> Grid -> Path
walkPath _ [] = []
walkPath s g  = head (head g) : walkPath s (shiftGrid s g)

countTrees = length . filter id

partOne = countTrees . walkPath (3,1)
partTwo g = product . map countTrees $ walkPath <$> slopesToTest <*> pure g

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
