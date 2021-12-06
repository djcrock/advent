import Data.Char ( digitToInt, isDigit )
import qualified Data.Map as Map

-- A School maps (timer value) -> (number of fish)
type School = Map.Map Int Int

parse :: String -> School
parse = toSchool . map digitToInt . filter isDigit
    where toSchool = foldr (\timer -> Map.insertWith (+) timer 1) Map.empty

tick :: School -> School
tick = Map.foldrWithKey ageFish Map.empty
    where ageFish timer count 
              | timer == 0 = Map.insertWith (+) 6 count . Map.insert 8 count
              | otherwise  = Map.insertWith (+) (timer - 1) count

countFishAtDay :: Int -> School -> Int
countFishAtDay day = Map.foldr (+) 0 . (!! day) . iterate tick

partOne = countFishAtDay 80
partTwo = countFishAtDay 256

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
