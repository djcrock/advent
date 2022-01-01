import Advent

generatePasswords :: ([Int],[Int]) -> [[Int]]
generatePasswords ([],[]) = [[]]
generatePasswords ((minDigit:minDigits),(maxDigit:maxDigits)) = do
    digit <- [minDigit..maxDigit]
    let minDigits' = if digit == minDigit
                        then zipWith max minDigits (repeat digit)
                        else replicate (length minDigits) digit
    let maxDigits' = if digit == maxDigit
                        then maxDigits
                        else replicate (length maxDigits) 9
    rest <- generatePasswords (minDigits',maxDigits')
    pure $ digit:rest

numValidPasswords :: ([Int] -> Bool) -> ([Int],[Int]) -> Int
numValidPasswords pred  = length . filter pred . generatePasswords

partOne = numValidPasswords (any ( > 1) . runLengths)
partTwo = numValidPasswords (any (== 2) . runLengths)

main = runSolutions [partOne, partTwo] (both intToDigits . read)
