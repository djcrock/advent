import Advent
import Data.List ( permutations )
import Data.Maybe ( mapMaybe )

permuteInputs :: Int -> Int -> Computer -> [[Computer]]
permuteInputs low high = sequence $ map sequence inputs
    where inputs = map (map pushInput) (permutations [low..high])

stepChain :: ([Computer], Maybe Int) -> Computer -> ([Computer], Maybe Int)
stepChain (prev,prevOutput) c = case prevOutput of
    Nothing    -> (prev ++ [c ], Nothing)
    Just x     -> (prev ++ [c'], out)
        where (out,c') = popOutput $ run $ pushInput x c

runChain :: ([Computer], Maybe Int) -> ([Computer], Maybe Int)
runChain (cs,val) = foldl stepChain ([],val) cs

runChainUntilHalt :: Int -> [Computer] -> Int
runChainUntilHalt n cs = case runChain (cs,Just n) of
    (cs',Nothing) -> n
    (cs',Just x)  -> runChainUntilHalt x cs'

partOne = maximum . mapMaybe (snd . runChain . (,Just 0)) . permuteInputs 0 4
partTwo = maximum . map (runChainUntilHalt 0) . permuteInputs 5 9

main = runSolutions [partOne, partTwo] readComputer
