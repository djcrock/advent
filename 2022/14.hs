import Text.Parsec
import qualified Data.Map as Map

type Point = (Int, Int)
type Path = [Point]
type Grid = Map.Map Point Char

parseInput :: String -> Grid
parseInput = toGrid . must . parse (sepBy pathP newline) ""
    where toGrid = Map.fromList . map (,'#') . concatMap pathToPoints
          must   = either (error . show) id
          pathP  = sepBy pointP (string " -> ")
          pointP = (,) <$> natP <* char ',' <*> natP
          natP   = read <$> many1 digit

pathToPoints :: Path -> [Point]
pathToPoints []                   = []
pathToPoints [(c,r)]              = [(c,r)]
pathToPoints ((c1,r1):(c2,r2):ps) = [ (c,r)
                                    | c <- [(min c1 c2) .. (max c1 c2) ]
                                    , r <- [(min r1 r2) .. (max r1 r2) ] ]
                                    ++ pathToPoints ((c2,r2):ps)

lastRow :: Grid -> Int
lastRow = maximum . map snd . Map.keys

-- Iterate a function until the result stops changing, returning the final value
fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == f x then x else fix f (f x)

dropSandFall :: Int -> Point -> Grid -> Grid
dropSandFall bottom (c,r) g = if r > bottom then g else
    case filter (not . flip Map.member g) [(c,r+1), (c-1,r+1), (c+1,r+1)] of
        []  -> Map.insert (c,r) 'o' g
        x:_ -> dropSandFall bottom x g

dropSandFloor :: Int -> Point -> Grid -> Grid
dropSandFloor bottom (c,r) g = if r == bottom then Map.insert (c,r) 'o' g else
    case filter (not . flip Map.member g) [(c,r+1), (c-1,r+1), (c+1,r+1)] of
        []  -> Map.insert (c,r) 'o' g
        x:_ -> dropSandFloor bottom x g

countSand :: Grid -> Int
countSand = Map.size . Map.filter (=='o')

partOne g = countSand $ fix (dropSandFall (lastRow g) (500,0)) g
partTwo g = countSand $ fix (dropSandFloor (1 + lastRow g) (500,0)) g

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parseInput
