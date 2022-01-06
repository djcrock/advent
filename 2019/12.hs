import Advent
import Data.List ( delete, transpose )

type Moon = ([Int],[Int])

foldr1AgainstRest :: Eq a => (a -> a -> a) -> [a] -> [a]
foldr1AgainstRest f xs = map (\x -> foldr f x (delete x xs)) xs

accel :: Moon -> Moon -> Moon
accel (toPos,_) (fromPos,vel) =
    ( fromPos
    , zipWith (+) vel $ zipWith ((signum .) . (-)) toPos fromPos )

move :: Moon -> Moon
move (pos,vel) = (zipWith (+) pos vel, vel)

tick :: [Moon] -> [Moon]
tick = map move . foldr1AgainstRest accel

cycleLength :: [Moon] -> Int
cycleLength xs = (+1) $ length $ takeWhile (/= xs) $ tail $ iterate tick xs

oneDimensional :: Moon -> [Moon]
oneDimensional ([],[])     = []
oneDimensional (p:ps,v:vs) = ([p],[v]) : oneDimensional (ps,vs)
    
energy :: Moon -> Int
energy = uncurry (*) . both (sum . map abs)

partOne = sum . map energy . (!! 1000) . iterate tick
partTwo = foldr1 lcm . map cycleLength . transpose . map oneDimensional

main = runSolutions [partOne, partTwo] (map posToMoon . readLines)
    where posToMoon xs = (xs,replicate (length xs) 0)
