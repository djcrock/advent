import Advent

main = runSolutions [runWithIO 1, runWithIO 5] readComputer
    where runWithIO input = head . outputs . run . setInputs [input]
