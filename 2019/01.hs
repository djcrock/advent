import Advent

fuelCost :: Int -> Int
fuelCost weight = max 0 ((div weight 3) - 2)

partOne = sum . map fuelCost
partTwo = sum . concatMap (tail . iterateFix fuelCost)

main = runSolutions [partOne, partTwo] readLines
