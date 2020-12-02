import Text.Parsec
    ( char,
      digit,
      letter,
      newline,
      space,
      many1,
      sepEndBy,
      skipMany1,
      parse,
      Parsec )

type Password = (Int, Int, Char, String)

passwordParser :: Parsec String () Password
passwordParser = do
    min <- many1 digit
    char '-'
    max <- many1 digit
    skipMany1 space
    ch <- letter
    char ':' 
    skipMany1 space
    pass <- many1 letter
    return (read min, read max, ch, pass)

inputParser :: Parsec String () [Password]
inputParser = sepEndBy passwordParser newline

must (Right passwords) = passwords

isValidPartOne :: Password -> Bool
isValidPartOne (min, max, ch, pass) = min <= reps && reps <= max
    where reps = length . filter (==ch) $ pass

isValidPartTwo :: Password -> Bool
isValidPartTwo (min, max, ch, pass) = reps == 1
    where reps = length . filter (==ch) $ [pass !! (min - 1), pass !! (max - 1)]

partOne = length . filter isValidPartOne
partTwo = length . filter isValidPartTwo

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . must . parse inputParser ""