import Advent

runBOOST :: Int -> Computer -> Int
runBOOST x = head . toListQ . outputs . run . pushInput x

main = runSolutions [runBOOST 1, runBOOST 2] readComputer
