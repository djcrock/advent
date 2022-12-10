parse:: String -> [Maybe Int]
parse = map (toInstruction . words) . lines
    where toInstruction ["noop"]      = Nothing
          toInstruction ["addx", val] = Just (read val)

run :: Int -> [Maybe Int] -> [Int]
run reg []              = []
run reg (Nothing:xs)    = reg : run reg xs
run reg ((Just val):xs) = reg : reg : run (reg+val) xs

chunk :: Int -> [a] -> [[a]]
chunk n xs = case splitAt n xs of
    (curr, [])   -> [curr]
    (curr, rest) -> curr : chunk n rest

drawSprite :: Int -> Int -> Char
drawSprite crt sprite = if abs (crt-sprite) <= 1 then '#' else '.'

partOne = show . sum . map (uncurry (*) . head) . chunk 40 . drop 19 . zip [1..] . run 1
partTwo = unlines . map (zipWith drawSprite [0..]) . chunk 40 . run 1

main = interact $ unlines . sequence [partOne, partTwo] . parse
