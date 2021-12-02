parse :: String -> [Int]
parse = map read . lines

diffs :: [Int] -> [Int]
diffs elevations = zipWith (-) (tail elevations) elevations

window :: [Int] -> [Int]
window (x:y:z:rest) = (x + y + z) : window (y:z:rest)
window _ = []

partOne = length . filter (>0) . diffs
partTwo = length . filter (>0) . diffs . window

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
