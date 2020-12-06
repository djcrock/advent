import Data.List ( intersect, union )
import Data.List.Split ( splitOn )

parse :: String -> [[String]]
parse = map lines . splitOn "\n\n"

partOne = sum . map (length . foldr1 union)
partTwo = sum . map (length . foldr1 intersect)

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
