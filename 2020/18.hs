import Text.Parsec
import Text.Parsec.Expr

data Expr = Add Expr Expr
          | Mul Expr Expr
          | Val Int
          deriving (Show)

type Parser a = Parsec String () a 

eval :: Expr -> Int
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y
eval (Val x)   = x

natP :: Parser Int
natP = read <$> many1 digit

makeExprParser table = exprP
    where exprP = buildExpressionParser table termP
          termP = (char '(' *> exprP <* char ')') <|> (Val <$> natP)

mul = Infix (try $ spaces >> char '*' >> spaces >> return Mul) AssocLeft
add = Infix (try $ spaces >> char '+' >> spaces >> return Add) AssocLeft

must (Right x) = x
must (Left y)  = error $ show y

parseExprs :: Parser Expr -> String -> [Expr]
parseExprs p = map (must . parse p "") . lines

partOne = sum . map eval . parseExprs (makeExprParser [[add,mul]])
partTwo = sum . map eval . parseExprs (makeExprParser [[add],[mul]])

main = interact $ (++ "\n") . show . sequence [partOne, partTwo]
