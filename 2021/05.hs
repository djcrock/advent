import Text.Parsec
import qualified Data.Map as Map

type Parser a = Parsec String () a
type Point = (Int, Int)
type Segment = (Point, Point)
type Grid = Map.Map Point Int

inputParser :: Parser [Segment]
inputParser = sepEndBy segmentP newline
    where segmentP = (,) <$> (pointP <* string " -> ") <*> pointP
          pointP   = (,) <$> (natP <* char ',') <*> natP
          natP     = read <$> many1 digit

must (Right x) = x
must (Left y)  = error $ show y

isAxial :: Segment -> Bool
isAxial ((x1,y1), (x2,y2)) = x1 == x2 || y1 == y2

toPoints :: Segment -> [Point]
toPoints ((x1,y1), (x2,y2))
    | x1 == x2  = [(x1, y) | y <- [(min y1 y2)..(max y1 y2)]]
    | y1 == y2  = [(x, y1) | x <- [(min x1 x2)..(max x1 x2)]]
    | otherwise = [(x1+dx, y1+((abs dx)*(signum $ y2-y1))) | dx <- [0, (signum $ x2-x1)..(x2-x1)]]

addToGrid :: Segment -> Grid -> Grid
addToGrid seg g = foldr (\point -> Map.insertWith (+) point 1) g (toPoints seg)

plot :: [Segment] -> Grid
plot = foldr addToGrid Map.empty

countOverlaps :: Grid -> Int
countOverlaps = Map.size . Map.filter (>1)

partOne = countOverlaps . plot . filter isAxial
partTwo = countOverlaps . plot

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . must . parse inputParser ""
