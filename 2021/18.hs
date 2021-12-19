import Control.Applicative ( (<|>) )
import Data.Functor ( (<&>) )
import Text.Parsec ( char, digit, many1, newline, parse, sepEndBy, try )

data Number = Regular Int | Pair Number Number deriving Eq

parseInput :: String -> [Number]
parseInput = must . parse (sepEndBy numP newline) ""
    where numP  = try regP <|> pairP
          pairP = Pair <$> (char '[' *> numP <* char ',') <*> (numP <* char ']')
          regP  = Regular <$> read <$> many1 digit
          must (Right x) = x
          must (Left y)  = error $ show y

-- Returns Just if value is changed, Nothing otherwise
explode' :: Int -> Number -> Maybe (Number, (Int, Int))
explode' _ (Regular x) = Nothing
explode' 4 (Pair (Regular l) (Regular r)) = Just (Regular 0, (l,r))
explode' d (Pair l r) =
        (explode' (d+1) l <&> (\(n, (l',r')) -> (Pair n (addL r' r), (l',0))))
    <|> (explode' (d+1) r <&> (\(n, (l',r')) -> (Pair (addR l' l) n, (0,r'))))
    where addL n (Regular x) = Regular (x+n)
          addL n (Pair l r)  = Pair (addL n l) r
          addR n (Regular x) = Regular (x+n)
          addR n (Pair l r)  = Pair l (addR n r)

-- Returns Just if value is changed, Nothing otherwise
split' :: Number -> Maybe Number
split' (Regular x) = if x < 10 then Nothing else
    Just $ Pair (Regular (div x 2)) (Regular ((div x 2) + (mod x 2)))
split' (Pair l r) = (split' l <&> (\n -> Pair n r))
                <|> (split' r <&> (\n -> Pair l n))

add :: Number -> Number -> Number
add l r = reduce (Pair l r)
    where reduce = fix (split . fix explode)
          fix f x = if x == f x then x else fix f (f x)
          explode num = maybe num fst (explode' 0 num)
          split num = maybe num id (split' num)

magnitude :: Number -> Int
magnitude (Regular x) = x
magnitude (Pair l r)  = (3 * magnitude l) + (2 * magnitude r)

partOne = magnitude . foldl1 add
partTwo = maximum . map magnitude . pairs
    where pairs nums = [add x y | x <- nums, y <- nums, x /= y]

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parseInput
