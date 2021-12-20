type Image = [[Bool]]

parseInput :: String -> ([Bool],Image)
parseInput = (\x -> (head x, drop 2 x)) . map (map (=='#')) . lines

-- The "flood" value represents the infinite surrounding space
enhancements :: Bool -> [Bool] -> Image -> [Image]
enhancements flood lut img = img : enhancements nextFlood lut enhanced
    where enhanced  = [[ lut !! bitsToInt [
                        maybe flood id ((img !? sr) >>= (!? sc))
                        | sr <- [r-1..r+1], sc <- [c-1..c+1] ]
                        | c  <- [-1..length $ head img] ]
                        | r  <- [-1..length img] ]
          nextFlood = lut !! (bitsToInt (replicate 9 flood))
          bitsToInt = foldl (\int bit -> int * 2 + fromEnum bit) 0
          xs !? n   = if n<0 || n>=(length xs) then Nothing else Just (xs !! n)

litAt :: Int -> ([Bool],Image) -> Int
litAt n = length . filter id . concat . (!! n) . uncurry (enhancements False)

main = interact $ (++ "\n") . show . sequence [litAt 2, litAt 50] . parseInput
