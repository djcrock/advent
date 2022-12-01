import Advent
import Data.List ( sort )

parse :: String -> [[Int]]
parse = map (map read . lines) . splitOnList "\n\n"

partOne = maximum . map sum
partTwo = sum . take 3 . reverse . sort . map sum

main = runSolutions [partOne, partTwo] parse
