import Data.List ( sort )
import Text.Parsec
import qualified Data.Map as Map

type Parser a = Parsec String () a
type Item = Int
type Operation = (Int -> Int)
type Monkey = ([Item], Operation, Int, (MonkeyId, MonkeyId), Int)
newtype MonkeyId = MonkeyId Int deriving ( Eq, Ord, Show )
type Monkeys = Map.Map MonkeyId Monkey

parseInput :: String -> Monkeys
parseInput = must . parse (Map.fromList <$> sepBy monkeyP newline) ""
    where must = either (error . show) id

natP :: Parser Int
natP = read <$> (many1 digit)

operationP :: Parser Operation
operationP = eval <$> (string "  Operation: new = " *> termP)
                  <*> (space *> operatorP)
                  <*> (space *> termP)
                  <*  newline
    where termP        = try (pure <$> natP) <|> (pure id <* string "old")
          operatorP    = try (pure (+) <* char '+') <|> (pure (*) <* char '*')
          eval l f r x = f (l x) (r x)

monkeyP :: Parser (MonkeyId, Monkey)
monkeyP = (,) <$> (string "Monkey " *> monkeyIdP <* string ":\n")
              <*> ((,,,,) <$> itemsP <*> operationP <*> testP <*> targetsP <*> pure 0)
    where monkeyIdP = MonkeyId <$> natP
          itemsP    = string "  Starting items: " *> sepBy natP (string ", ") <* newline
          testP     = string "  Test: divisible by " *> natP <* newline
          targetsP  = (,) <$> (string "    If true: throw to monkey "  *> monkeyIdP <* newline)
                          <*> (string "    If false: throw to monkey " *> monkeyIdP <* newline)

throw :: Monkey -> Monkey
throw (x:xs, op, t, (l, r), throws) = (xs, op, t, (l, r), throws+1)

catch :: Item -> Monkey -> Monkey
catch item (xs, op, t, (l, r), throws) = (xs ++ [item], op, t, (l, r), throws)

turn :: (Int -> Int) -> Monkeys -> MonkeyId -> Monkeys
turn worryDown ms mid = case ms Map.! mid of
    m@([], _, _, _, _)               -> ms
    m@(item:items, op, t, (l, r), _) -> turn worryDown ms'' mid
        where item'  = worryDown (op item)
              target = if mod item' t == 0 then l else r
              ms'    = Map.insert mid (throw m) ms
              ms''   = Map.adjust (catch item') target ms'

play :: (Int -> Int) -> Monkeys -> Monkeys
play worryDown ms = foldl (turn worryDown) ms (Map.keys ms)

monkeyBusiness :: Monkeys -> Int
monkeyBusiness = product . take 2 . reverse . sort . map getThrows . Map.elems
    where getThrows (_, _, _, _, throws) = throws

-- All monkeys decide to throw based on divisibility by prime factors.
-- By taking the product of all these prime factors, we find the maximum number
-- that might "matter" for the purpose of making a decision. We can safely use
-- modulus to manage our worry without affecting the behavior of the monkeys.
manageWorry :: Monkeys -> Int -> Int
manageWorry ms worry = mod worry (product $ map getTest $ Map.elems ms)
    where getTest (_, _, t, _, _) = t

partOne    = monkeyBusiness . (!! 20)    . iterate (play (`div` 3))
partTwo ms = monkeyBusiness $ (!! 10000) $ iterate (play (manageWorry ms)) ms

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parseInput
