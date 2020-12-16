import Data.List
import Text.Parsec

type Parser a = Parsec String () a
type Range = (Int,Int)
type Rule = (String,[Range])
type Ticket = [Int]

natP :: Parser Int
natP = read <$> many1 digit

rangeP :: Parser Range
rangeP = do
    minVal <- natP
    char '-'
    maxVal <- natP
    return (minVal,maxVal)

ruleP :: Parser Rule
ruleP = do
    ruleName <- manyTill anyChar (string ": ")
    ranges <- sepBy rangeP (string " or ")
    newline
    return (ruleName,ranges)

ticketP :: Parser Ticket
ticketP = sepBy natP (char ',') <* newline

inputParser :: Parser ([Rule],Ticket,[Ticket])
inputParser = do
    rules <- manyTill ruleP newline
    string "your ticket:" >> newline
    myTicket <- ticketP
    newline >> string "nearby tickets:" >> newline
    nearbyTickets <- many ticketP
    return (rules, myTicket, nearbyTickets)

must (Right x) = x
must (Left y)  = error $ show y

satisfies :: Rule -> Int -> Bool
satisfies (_,ranges) val = any (between val) ranges
    where between val (min,max) = val >= min && val <= max

satisfiesAny :: [Rule] -> Int -> Bool 
satisfiesAny rules val = or (satisfies <$> rules <*> pure val)

allSatisfy :: [Int] -> Rule -> Bool
allSatisfy vals rule = all (satisfies rule) vals

-- For each field position in the given tickets, find the rules that could
-- apply to it. Results are sorted by the number of rules that could apply
-- at the position.
getPossibleRules :: [Rule] -> [Ticket] -> [(Int,[Rule])]
getPossibleRules rules tickets = sortOn (length . snd) rulesByPosition
    where validTickets       = filter (all (satisfiesAny rules)) tickets
          fields             = transpose validTickets
          getValidRules vals = filter (allSatisfy vals) rules
          rulesByPosition    = zip [0..] $ map getValidRules fields

-- Given a list of (position,[validRule]), sorted by the number of valid rules,
-- determine which SPECIFIC rule applies at each position. After a rule has been
-- "claimed" by a position, it is considered "solved" and can no loger be used
-- in any other position. This narrows down which rules can be used at the
-- remaining positions, allowing them to be solved in turn etc.
solveRules :: [Rule] -> [(Int,[Rule])] -> [(Int,Rule)]
solveRules _      []                 = []
solveRules solved ((pos,rules):rest) = (pos,rule) : solveRules (rule:solved) rest
    where rule = head $ filter (not . (`elem` solved)) rules

getFieldOrder :: [Rule] -> [Ticket] -> [String]
getFieldOrder rules tickets = map (fst . snd) $ sortOn fst solvedRules
    where solvedRules = solveRules [] (getPossibleRules rules tickets)

getAtIndices :: [Int] -> [a] -> [a]
getAtIndices []              _  = []
getAtIndices (index:indices) xs = (xs !! index) : getAtIndices indices xs

partOne (rules,_,nearby) = sum $ filter (not . satisfiesAny rules) $ concat nearby
partTwo (rules,myTicket,nearby) = product (getAtIndices departureIndices myTicket)
    where fieldOrder       = getFieldOrder rules nearby
          departureIndices = findIndices (isPrefixOf "departure") fieldOrder

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . must . parse inputParser ""
