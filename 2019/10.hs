import Advent
import Data.Fixed ( mod' )
import Data.List ( delete, sortOn )

type Asteroid = Vec2
type Angle = Vec2

parseInput :: String -> [Asteroid]
parseInput input = map fst $ filter ((=='#') . snd) $ zip coords (deleteAll '\n' input)
    where coords =
            [ (x,y)
            | y <- [0..(length $ lines input)-1]
            , x <- [0..(length $ head $ lines input)-1] ]

angle :: Asteroid -> Asteroid -> Angle
angle (x1,y1) (x2,y2) = both (`div` (gcd dx dy)) (dx,dy)
    where dx = (x2 - x1)
          dy = (y2 - y1)

laserAngle :: Angle -> Float
laserAngle (x,y) = (`mod'` (2*pi)) $ ((2*pi)-)
                    $ if ang < 0 then ang + 2*pi else ang
    where ang = atan2 (fromIntegral (-x)) (fromIntegral (-y))

asteroidAngles :: [Asteroid] -> Asteroid -> [(Angle,[Asteroid])]
asteroidAngles asts ast = sortOn (laserAngle . fst)
                            $ map sortByDistance
                            $ categorize (angle ast) (delete ast asts)
    where sortByDistance (ang,asts') = (ang, sortOn (manhattan ast) asts')

bestAsteroidAngles :: [Asteroid] -> [(Angle,[Asteroid])]
bestAsteroidAngles asts = maximumOn length $ map (asteroidAngles asts) asts

partOne = length . bestAsteroidAngles
partTwo = (\(x,y) -> 100*x + y) . head . snd . (!! 199) . bestAsteroidAngles

main = runSolutions [partOne, partTwo] parseInput
