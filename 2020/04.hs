import Control.Applicative ( Alternative((<|>)) )
import Data.Char ( isDigit, isHexDigit )
import qualified Data.Map as Map
import Text.Parsec
    ( char,
      digit,
      newline,
      space,
      many1,
      noneOf,
      sepEndBy,
      string,
      parse,
      try,
      Parsec )
import Text.Read (readMaybe)

data LengthUnit = Inch | Centimeter deriving Show

data Field = BirthYear
           | IssueYear
           | ExpirationYear
           | Height
           | HairColor
           | EyeColor
           | PassportId
           | CountryId
           deriving (Show, Eq, Ord)

type Passport = Map.Map Field String

fieldParser :: Parsec String () Field
fieldParser = 
        try (string "byr" >> return BirthYear)
    <|> try (string "iyr" >> return IssueYear)
    <|> try (string "eyr" >> return ExpirationYear)
    <|> try (string "hgt" >> return Height)
    <|> try (string "hcl" >> return HairColor)
    <|> try (string "ecl" >> return EyeColor)
    <|> try (string "pid" >> return PassportId)
    <|> try (string "cid" >> return CountryId)

pairParser :: Parsec String () (Field, String)
pairParser = do
    field <- fieldParser
    char ':'
    value <- many1 $ noneOf " \n"
    return (field, value)

passportParser :: Parsec String () Passport
passportParser = do
    pairs <- sepEndBy pairParser (newline <|> space)
    return $ Map.fromList pairs

inputParser :: Parsec String () [Passport]
inputParser = sepEndBy passportParser (newline <|> space)

heightParser :: Parsec String () (Int, LengthUnit)
heightParser = do
    height <- many1 digit
    unit <- (string "in" >> return Inch) <|> (string "cm" >> return Centimeter)
    return (read height, unit)

validateYear :: Int -> Int -> String -> Bool
validateYear min max xs = Just True == do
    year <- readMaybe xs
    return $ year >= min && year <= max

validateHeight hgt = case parse heightParser "" hgt of
    (Left _)                     -> False
    (Right (height, Inch))       -> height >= 59 && height <= 76
    (Right (height, Centimeter)) -> height >= 150 && height <= 193

validateHairColor hcl =
    head hcl == '#'
    && all isHexDigit (tail hcl)
    && length hcl == 7

validateEyeColor = flip elem ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validatePassportId pid = all isDigit pid && length pid == 9

validateField :: Field -> String -> Bool
validateField BirthYear      = validateYear 1920 2002
validateField IssueYear      = validateYear 2010 2020
validateField ExpirationYear = validateYear 2020 2030
validateField Height         = validateHeight
validateField HairColor      = validateHairColor
validateField EyeColor       = validateEyeColor
validateField PassportId     = validatePassportId
validateField CountryId      = const True

allFieldsValid :: Passport -> Bool
allFieldsValid = Map.foldrWithKey f True
    where f k a b = b && validateField k a

hasFields :: [Field] -> Passport -> Bool
hasFields fs p = and $ map Map.member fs <*> pure p

requiredFields = [ BirthYear
                 , IssueYear
                 , ExpirationYear
                 , Height
                 , HairColor
                 , EyeColor
                 , PassportId
                 ]

hasRequiredFields :: Passport -> Bool
hasRequiredFields = hasFields requiredFields

partOne = length . filter hasRequiredFields
partTwo = length . filter allFieldsValid . filter hasRequiredFields

must (Right x) = x
must (Left y)  = error $ show y

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . must . parse inputParser ""
