import Advent

main = runSolutions [runWithIO 1, runWithIO 5] readComputer
    where runWithIO input = last . toListQ . outputs . run . setInputs [input]
