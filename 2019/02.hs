import Advent

runNounVerb :: Int -> Int -> Computer -> Int
runNounVerb noun verb = peek 0 . run . poke 2 verb . poke 1 noun

partOne = runNounVerb 12 2
partTwo computer = head
    [ noun * 100 + verb
    | noun <- [0..99]
    , verb <- [0..99]
    , runNounVerb noun verb computer == 19690720 ]

main = runSolutions [partOne, partTwo] readComputer
