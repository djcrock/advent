import Advent
import Data.List ( (\\) )
import qualified Data.Map as Map

parseInput :: String -> Map.Map String String
parseInput = Map.fromList . map ((\[x,y] -> (y,x)) . splitOn ')') . lines

path :: Map.Map String String -> String -> [String]
path os item = maybe [] ((item:) . path os) (Map.lookup item os)

partOne os = sum $ map (length . path os) (Map.keys os)
partTwo os = length $ disjoint (path os "YOU") (path os "SAN") \\ ["YOU","SAN"]

main = runSolutions [partOne, partTwo] parseInput
