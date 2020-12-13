import Control.Applicative
import Control.Arrow ( second )
import Data.List ( find, sortOn )
import Data.List.Split ( splitOn )
import Data.Maybe ( catMaybes, fromJust )
import Text.Read ( readMaybe )

parse :: String -> (Int,[Maybe Int])
parse = p . lines
    where p [time,buses] = (read time, parseBuses buses)
          parseBuses     = map readMaybe . splitOn ","

-- Get the ID of the next bus, along with the number of minutes until it departs
getNext :: (Int,[Int]) -> (Int,Int)
getNext (time,buses) = second (subtract time) $ head nextArrivals
    where nextArrivals = sortOn snd $ map arrival buses
          arrival bus = (bus, fromJust $ find (>= time) (map (*bus) [0..]))

withPositions :: [Maybe Int] -> [(Int,Int)]
withPositions = catMaybes . zipWith (liftA2 (,)) (map pure [0..])

-- Step the time until the first bus lands in its appropriate position.
-- Then, multiply the step size by that bus' "stride" so that all subsequent
-- steps result in that bus being in position. Continue for the remaining
-- buses until they are all "locked in", then return the final timestamp.
findWinner :: Int -> Int -> [(Int,Int)] -> Int
findWinner time _    []                     = time
findWinner time step ((position,bus):buses) =
    let next = time + step in
    if mod (next + position) bus == 0
        then findWinner next (step * bus) buses
        else findWinner next step         ((position,bus):buses)

partOne = uncurry (*) . getNext . second catMaybes
partTwo = findWinner 0 1 . withPositions . snd

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
