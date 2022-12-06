import Data.List ( nub )

charsTillMarker :: Int -> String -> Int
charsTillMarker n = (+n) . length . takeWhile ((<n) . length . nub) . window n
    where window n xs = (take n xs) : window n (tail xs)

partOne = charsTillMarker 4
partTwo = charsTillMarker 14

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . init
