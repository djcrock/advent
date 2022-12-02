import Data.Char ( ord )

-- Map both "ABC" and "XYZ" to [0,1,2]
parse :: String -> [(Int, Int)]
parse = map (\[x,_,y] -> (ord x - ord 'A', ord y - ord 'X')) . lines

score :: (Int, Int) -> Int
score (them, me) = 1 + me + [3, 6, 0] !! ((me - them) `mod` 3)

forceOutcome :: (Int, Int) -> (Int, Int)
forceOutcome (them, outcome) = (them, (them + outcome - 1) `mod` 3)

partOne = sum . map score
partTwo = sum . map (score . forceOutcome)

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
