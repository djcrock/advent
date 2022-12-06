import Data.List ( transpose )
import Data.Maybe ( catMaybes )
import Text.Parsec

type Stacks = [[Char]]
type Instruction = (Int, Int, Int)

parseInput :: String -> (Stacks, [Instruction])
parseInput = must . parse ((,) <$> (rowsP <* lineP <* newline) <*> sepEndBy instP newline) ""
    where instP  = (,,) <$> (string "move " *> natP) <*> (string " from " *> natP) <*> (string " to " *> natP)
          lineP  = endBy (many1 (try (char ' ') <|> digit)) newline
          rowsP  = (map catMaybes . transpose) <$> sepEndBy (sepBy1 crateP (char ' ')) newline
          crateP = try fullP <|> try emptyP
          fullP  = Just <$> (char '[' *> anyChar <* char ']')
          emptyP = Nothing <$ string "   "
          natP   = read <$> many1 digit
          must   = either (error . show) id

replace :: Int -> a -> [a] -> [a]
replace 0 x (_:ys) = x:ys
replace n x (y:ys) = y : replace (n-1) x ys

pop :: Int -> Stacks -> (Char, Stacks)
pop 1 ((x:xs):rest) = (x, xs:rest)
pop n (xs:rest) = fmap (xs:) (pop (n-1) rest)

push :: Int -> Char -> Stacks -> Stacks
push 1 c (xs:rest) = (c:xs):rest
push n c (xs:rest) = xs : (push (n-1) c rest)

doStep :: ([Char] -> [Char]) -> Stacks -> Instruction -> Stacks
doStep crane stacks (n, src, dst) = stacks''
    where (mov, rest) = splitAt n (stacks !! (src-1))
          stacks'     = replace (src-1) rest stacks
          stacks''    = replace (dst-1) (crane mov ++ (stacks' !! (dst-1))) stacks'

partOne = head . transpose . uncurry (foldl (doStep reverse))
partTwo = head . transpose . uncurry (foldl (doStep id))

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parseInput
