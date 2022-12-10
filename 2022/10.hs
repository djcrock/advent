import Text.Parsec

data Instruction = Addx Int | Noop deriving ( Show ) 

parseInput :: String -> [Instruction]
parseInput = must . parse (sepEndBy instP newline) ""
    where instP = try addxP <|> noopP
          addxP = Addx <$> (string "addx " *> numP)
          noopP = Noop <$ string "noop"
          numP  = try natP <|> (negate <$> (char '-' *> natP))
          natP  = read <$> many1 digit
          must  = either (error . show) id

run :: Int -> [Instruction] -> [Int]
run reg []              = []
run reg (Noop:xs)       = reg : run reg xs
run reg ((Addx val):xs) = reg : reg : run (reg+val) xs

chunk :: Int -> [a] -> [[a]]
chunk n xs = case splitAt n xs of
    (curr, [])   -> [curr]
    (curr, rest) -> curr : chunk n rest

drawSprite :: Int -> Int -> Char
drawSprite crt sprite = if abs (crt-sprite) <= 1 then '#' else '.'

partOne = show . sum . map (uncurry (*) . head) . chunk 40 . drop 19 . zip [1..] . run 1
partTwo = unlines . map (zipWith drawSprite [0..]) . chunk 40 . run 1

main = interact $ unlines . sequence [partOne, partTwo] . parseInput
