
parse :: String -> [Int]
parse = map read . lines

main = interact $ (++ "\n") . show . parse