-- See 24.input.scratch for reverse engineering
data Op = Push Int | Pop
program =
    [ Push (-5)
    , Push (-1)
    , Push (-8)
    , Pop
    , Push   8
    , Pop
    , Push (-3)
    , Push   6
    , Pop
    , Push   5
    , Pop
    , Pop
    , Pop
    , Pop ]

run :: ([Int] -> Int) -> [Op] -> [Int] -> [Int]
run _ [] _                   = []
run sel (Pop:ops) (d:stack)  = d   : run sel ops stack
run sel ((Push n):ops) stack = val : run sel ops ((val+n):stack)
    where val = sel $ filter ((`elem` [1..9]) . (+n)) [1..9]

solve :: ([Int] -> Int) -> Int
solve sel = foldl1 (\acc i -> 10 * acc + i) $ run sel program []

main = interact $ (++ "\n") . show . pure [solve maximum, solve minimum]
