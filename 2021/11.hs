import Data.Char ( digitToInt )
import Data.List ( (\\) )
import qualified Data.Map as Map

type Coordinate = (Int, Int)
type Grid = Map.Map Coordinate Int
type Flashes = [Coordinate]

parse :: String -> (Grid, Flashes)
parse = (, []) . gridMap . map (map digitToInt) . lines
    where gridMap g = foldr (\c -> Map.insert c (get g c)) Map.empty coords
          coords = [(r, c) | r <- [0..9], c <- [0..9]]
          get g (r, c) = g !! r !! c

adjacent :: Coordinate -> [Coordinate]
adjacent (r, c) =
    [ (r', c')
    | r' <- [(r-1)..(r+1)], r' >= 0 && r' <= 9
    , c' <- [(c-1)..(c+1)], c' >= 0 && c' <= 9
    , (r', c') /= (r, c) ]

-- Iterate a function until the result stops changing
fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == f x then x else fix f (f x)

flash :: (Grid, Flashes) -> (Grid, Flashes)
flash (g, fs) = (g', fs')
    where flashCoords = (Map.keys $ Map.filter (>9) g) \\ fs
          g' = foldr (Map.adjust (+1)) g (concatMap adjacent flashCoords)
          fs' = fs ++ flashCoords

step :: (Grid, Flashes) -> (Grid, Flashes)
step (g, _) = (grid, flashes)
    where (g', flashes) = fix flash (Map.map (+1) g, [])
          grid = foldr (\flashCoord -> Map.insert flashCoord 0) g' flashes

partOne = length . concatMap snd . take 101 . iterate step
partTwo = length . takeWhile ((<100) . length . snd) . iterate step

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
