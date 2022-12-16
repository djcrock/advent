import Data.List ( find, sort )
import Data.Maybe ( catMaybes )
import Text.Parsec

type Point = (Int, Int)
type Range = (Int, Int)

parseInput :: String -> [(Point, Point)]
parseInput = must . parse (sepEndBy sensorP newline) ""
    where must    = either (error . show) id
          sensorP = (,) <$> (string "Sensor at " *> pointP)
                        <*> (string ": closest beacon is at " *> pointP)
          pointP  = (,) <$> (string "x=" *> numP)
                        <*> (string ", y=" *> numP)
          numP    = option id (negate <$ char '-') <*> (read <$> many1 digit)

manhattan :: Point -> Point -> Int
manhattan (ax,ay) (bx,by) = abs (bx-ax) + abs (by-ay)

excludedAtY :: Int -> (Point, Point) -> [Range]
excludedAtY y (s@(sx,sy), b@(bx,by)) =
    if xdist < 0
        then []
        else [(sx - xdist, sx + xdist)]
    where dist  = manhattan s b
          ydist = abs (sy - y)
          xdist = dist - ydist

combineRanges :: [Range] -> [Range]
combineRanges = foldr combineRanges' [] . sort
    where combineRanges' x [] = [x]
          combineRanges' x@(x1,x2) (y@(y1,y2):ys) =
            if x2 > y2
                then combineRanges' x ys
                else if x2 >= y1
                    then (x1,y2):ys
                    else x:y:ys

rangeSize :: Range -> Int
rangeSize = uncurry subtract

excludedRangesX :: Int -> [(Point, Point)] -> [Range]
excludedRangesX n = combineRanges . concatMap (excludedAtY n)

legalSpot :: Int -> [(Point, Point)] -> Maybe Point
legalSpot y ps = find ((>1) . length . snd) ranges >>= toPoint
    where ranges = [(y, excludedRangesX y ps) | y <- [0..]]
          toPoint (y,(_,x):_) = Just (x+1,y)

freq :: Point -> Int
freq (x,y) = x * 4000000 + y

partOne = sum . map rangeSize . excludedRangesX 2000000
partTwo = freq . head . catMaybes . sequence (map legalSpot [0..])

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parseInput
