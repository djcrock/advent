import Control.Applicative ( (<|>) )
import qualified Data.Map as Map
import Text.Parsec
    ( anyChar,
      char,
      digit,
      newline,
      string,
      many1,
      manyTill,
      optional,
      sepBy,
      sepEndBy,
      space,
      parse,
      try,
      Parsec )

type Color = String
type BagQuantity = (Color, Int)
type Rules = Map.Map Color [BagQuantity]

colorParser :: Parsec String () Color
colorParser = manyTill anyChar (try $ string " bag" >> optional (char 's'))

bagQuantityParser :: Parsec String () BagQuantity
bagQuantityParser = do
    quantity <- many1 digit
    space
    color <- colorParser
    return (color, read quantity)

ruleParser :: Parsec String () (Color, [BagQuantity])
ruleParser = do
    color <- colorParser
    string " contain "
    quantities <- (string "no other bags" >> return [])
                  <|> sepBy bagQuantityParser (string ", ")
    char '.'
    return (color, quantities)

inputParser :: Parsec String () Rules
inputParser = do
    rules <- sepEndBy ruleParser newline
    return (Map.fromList rules)


canContain :: Rules -> Color -> Color -> Bool
canContain rules inner outer = not isEmpty && (directContains || childContains)
    where contents       = map fst (rules Map.! outer)
          isEmpty        = null contents
          directContains = elem inner contents
          childContains  = any (canContain rules inner) contents

countChildren :: Rules -> Color -> Int
countChildren rules color = 1 + sum (map childCount contents)
    where contents                = rules Map.! color
          childCount (col, quant) = quant * countChildren rules col

partOne rules = length $ filter (canContain rules "shiny gold") $ Map.keys rules
-- Don't count the outermost bag
partTwo rules = countChildren rules "shiny gold" - 1

must (Right x) = x
must (Left y)  = error $ show y

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . must . parse inputParser ""
