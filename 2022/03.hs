import Data.List ( intersect )

compartments :: String -> (String, String)
compartments xs = splitAt (length xs `div` 2) xs

segment :: Int -> [a] -> [[a]]
segment n xs = case splitAt n xs of
    (seg, [])   -> [seg]
    (seg, rest) -> seg : segment n rest

priority :: Char -> Int
priority c = fromEnum c - if c > 'Z' then 96 else 38

partOne = sum . map (priority . head . uncurry intersect . compartments)
partTwo = sum . map (priority . head . foldr1 intersect) . segment 3

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . lines
