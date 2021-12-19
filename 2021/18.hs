import Control.Applicative
import Text.Parsec hiding ( (<|>) )

data Number = Leaf Int | Pair Number Number deriving Eq

parseInput :: String -> [Number]
parseInput = must . parse (sepEndBy numP newline) ""
    where numP  = try leafP <|> pairP
          pairP = Pair <$> (char '[' *> numP <* char ',') <*> (numP <* char ']')
          leafP = Leaf <$> read <$> many1 digit
          must  = either (error . show) id

explode :: Int -> Number -> Maybe (Number,(Int,Int))
explode _ (Leaf x) = Nothing
explode 4 (Pair (Leaf l) (Leaf r)) = Just (Leaf 0, (l,r))
explode d (Pair l r) =
        ((\(n,(l',r')) -> (Pair n (addL r' r), (l',0))) <$> explode (d+1) l)
    <|> ((\(n,(l',r')) -> (Pair (addR l' l) n, (0,r'))) <$> explode (d+1) r)
    where addL n (Leaf x)   = Leaf (x+n)
          addL n (Pair l r) = Pair (addL n l) r
          addR n (Leaf x)   = Leaf (x+n)
          addR n (Pair l r) = Pair l (addR n r)

split :: Number -> Maybe Number
split (Leaf x) = if x < 10 then Nothing else
    Just $ Pair (Leaf (div x 2)) (Leaf ((div x 2) + (mod x 2)))
split (Pair l r) = (flip Pair r <$> split l) <|> (Pair l <$> split r)

add :: Number -> Number -> Number
add l r = reduce (Pair l r)
    where reduce       = fix (split' . fix explode')
          fix f x      = if x == f x then x else fix f (f x)
          explode' num = maybe num fst (explode 0 num)
          split' num   = maybe num id (split num)

magnitude :: Number -> Int
magnitude (Leaf x)   = x
magnitude (Pair l r) = (3 * magnitude l) + (2 * magnitude r)

partOne = magnitude . foldl1 add
partTwo nums = maximum $ [magnitude $ add x y | x <- nums, y <- nums, x /= y]

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parseInput
