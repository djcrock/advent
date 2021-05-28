import Control.Monad ( join )
import qualified Data.IntMap as Map
import Text.Parsec

type Parser a = Parsec String () a
type RuleID = Int
data Matcher = MatchChar Char
             | MatchRule RuleID
             deriving Show
type Option = [Matcher]
type Rule = [Option]

matcherP :: Parser Matcher
matcherP = matchCharP <|> matchRuleP
    where matchCharP = MatchChar <$> (char '"' *> anyChar <* char '"')
          matchRuleP = MatchRule . read <$> many1 digit

optionP :: Parser Option
optionP = sepEndBy matcherP (char ' ')

ruleP :: Parser (RuleID, Rule)
ruleP = do
    ruleID <- read <$> manyTill digit (string ": ")
    options <- sepBy optionP (string "| ")
    newline
    pure (ruleID, options)

inputParser :: Parser ([(RuleID, Rule)], [String])
inputParser = do
    rules <- manyTill ruleP newline
    strings <- many $ manyTill anyChar newline
    pure (rules, strings)


matcherToStrings :: Map.IntMap Rule -> Matcher -> [String]
matcherToStrings _ (MatchChar c)     = [[c]]
matcherToStrings rules (MatchRule r) = ruleToStrings rules r

ruleToStrings :: Map.IntMap Rule -> RuleID -> [String]
ruleToStrings rules ruleID = do
    let rule = rules Map.! ruleID
    option <- rule
    let expandedOption = map (matcherToStrings rules) option
    map join $ sequence expandedOption

must (Right x) = x
must (Left y)  = error $ show y

partOne input = length $ filter (`elem` validStrings) strings
    where rules        = Map.fromList $ fst input
          strings      = snd input
          validStrings = ruleToStrings rules 0
partTwo = partOne

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . must . parse inputParser ""
