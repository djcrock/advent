import Data.List
import Data.Maybe
import Text.Parsec
import qualified Data.Map as Map
import qualified Data.Set as Set

type Point = (Int,Int,Int)
type Scan = [Point]
type Transformation = (Point -> Point)
type Distances = Map.Map Point (Set.Set [Int])

parseInput :: String -> [Scan]
parseInput = must . parse (sepEndBy scannerP newline) ""
    where scannerP = commentP >> sepEndBy beaconP newline
          commentP = string "---" >> manyTill anyChar newline
          beaconP  = (,,) <$> (numP <* char ',') <*> (numP <* char ',') <*> numP
          numP     = try negP <|> natP
          negP     = negate <$> (char '-' *> natP)
          natP     = read <$> many1 digit
          must     = either (error . show) id

rotations :: [Transformation]
rotations =
    [ \(x,y,z) -> ( x, y, z)
    , \(x,y,z) -> (-y, x, z)
    , \(x,y,z) -> (-x,-y, z)
    , \(x,y,z) -> ( y,-x, z)
    , \(x,y,z) -> ( x,-z, y)
    , \(x,y,z) -> ( z, x, y)
    , \(x,y,z) -> (-x, z, y)
    , \(x,y,z) -> (-z,-x, y)
    , \(x,y,z) -> (-z, y, x)
    , \(x,y,z) -> (-y,-z, x)
    , \(x,y,z) -> ( z,-y, x)
    , \(x,y,z) -> ( y, z, x)
    , \(x,y,z) -> ( x,-y,-z)
    , \(x,y,z) -> ( y, x,-z)
    , \(x,y,z) -> (-x, y,-z)
    , \(x,y,z) -> (-y,-x,-z)
    , \(x,y,z) -> ( x, z,-y)
    , \(x,y,z) -> (-z, x,-y)
    , \(x,y,z) -> (-x,-z,-y)
    , \(x,y,z) -> ( z,-x,-y)
    , \(x,y,z) -> ( z, y,-x)
    , \(x,y,z) -> (-y, z,-x)
    , \(x,y,z) -> (-z,-y,-x)
    , \(x,y,z) -> ( y,-z,-x) ]

-- Determine the translation needed to turn the first point into the second
translation :: Point -> Point -> (Int,Int,Int)
translation (x1,y1,z1) (x2,y2,z2) = (x2-x1, y2-y1, z2-z1)

translate :: (Int,Int,Int) -> Point -> Point
translate (dx,dy,dz) (x,y,z) = (x+dx, y+dy, z+dz)

-- Determine the rotation that brings the snds in line with the fsts.
-- Points are "in line" if all of them have the same translation distance from
-- their matching points.
rotation :: [(Point,Point)] -> Maybe Transformation
rotation pairs = find alignsPairs rotations
    where alignsPairs r   = allEqual $ map (\(x,y) -> translation (r y) x) pairs
          allEqual (x:xs) = all (==x) xs

transformation :: Scan -> Scan -> Maybe Transformation
transformation target source = do
    let pairs = Map.toList $ matches target source
    _   <- if length pairs < 12 then Nothing else Just ()
    rot <- rotation pairs
    let tran  = translation (rot $ snd $ head pairs) (fst $ head pairs)
    Just $ translate tran . rot

-- The set of (x,y,z) component distances from a point to its neighbors uniquely
-- (in this puzzle, at least) identifies that point with respect to those
-- neighbors. By ordering the component distances by magnitude, they become
-- constant with respect to rotation. This allows them to be compared against
-- points measured from other frames of reference (i.e. other scanners).
distance :: Point -> Point -> [Int]
distance (x1,y1,z1) (x2,y2,z2) = sort [abs (x2-x1), abs (y2-y1), abs (z2-z1)]

distances :: Scan -> Distances
distances ps = Map.fromList $ map (\p -> (p, distanceSet p)) ps
    where distanceSet p = Set.fromList $ map (distance p) ps

matchingPoint :: Distances -> Set.Set [Int] -> Maybe Point
matchingPoint ds s = listToMaybe $ Map.keys $ Map.filter isMatch ds
    where isMatch = (>=12) . Set.size . Set.intersection s

matches :: Scan -> Scan -> Map.Map Point Point
matches s1 s2 = Map.mapMaybe (matchingPoint (distances s2)) (distances s1)

-- Reconcile scans of multiple overlapping volumes to a single set of points
-- All coordinates will be transformed to the reference frame of the first scan.
collate :: [Scan] -> [(Transformation,Scan)]
collate (x:xs) = collate' [(id,x)] xs

collate' :: [(Transformation,Scan)] -> [Scan] -> [(Transformation,Scan)]
collate' done []       = done
collate' [] uncollated = error $ "Uncollated scans remain" ++ show uncollated
collate' ((xform,target):rest) uncollated = (xform,target) : (collate' rest' uncollated')
    where transformations     = mapMaybe getTransformation uncollated
          getTransformation s = (,s) <$> (transformation target s)
          rest'               = rest ++ (map (\(f,s) -> (f, map f s)) transformations)
          uncollated'         = uncollated \\ (map snd transformations)

scannerPosition :: Transformation -> Point
scannerPosition f = f (0,0,0)

partOne = length . nub . concatMap snd . collate
partTwo = maximum . manhattans . map (scannerPosition . fst) . collate
    where manhattans xs = [sum $ distance x y | x <- xs, y <- xs]

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parseInput
