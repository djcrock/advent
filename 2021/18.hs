import Data.Maybe ( fromMaybe )
import Text.Parsec 

type Parser a = Parsec String () a
data Number = Regular Int | Pair Number Number deriving Eq

inputP :: Parser [Number]
inputP = sepEndBy numberP newline
    where numberP  = try regularP <|> pairP
          pairP    = Pair
                        <$> (char '[' *> numberP <* char ',')
                        <*> (numberP <* char ']')
          regularP = Regular <$> natP
          natP     = read <$> many1 digit

must (Right x) = x
must (Left y)  = error $ show y

-- Iterate a function until the result stops changing
fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == f x then x else fix f (f x)

reduce :: Number -> Number
reduce = fix (split . fix explode)

explode :: Number -> Number
explode num = case explode' 0 num of
    Nothing            -> num
    Just (exploded, _) -> exploded

-- Returns Just if value is changed, Nothing otherwise
explode' :: Int -> Number -> Maybe (Number, (Int, Int))
explode' _ (Regular x) = Nothing
explode' 4 (Pair (Regular l) (Regular r)) = Just (Regular 0, (l,r))
explode' depth (Pair l r) = case explode' (depth+1) l of
    Just (expL, (lVal,rVal)) -> Just (Pair expL (addL rVal r), (lVal,0))
    Nothing                  -> case explode' (depth+1) r of
        Just (expR, (lVal,rVal)) -> Just (Pair (addR lVal l) expR, (0,rVal))
        Nothing                  -> Nothing

-- Add to the given number's leftmost Regular value
addL :: Int -> Number -> Number
addL n (Regular x) = Regular (x+n)
addL n (Pair l r)  = Pair (addL n l) r

-- Add to the given number's rightmost Regular value
addR :: Int -> Number -> Number
addR n (Regular x) = Regular (x+n)
addR n (Pair l r)  = Pair l (addR n r)

split :: Number -> Number
split num = fromMaybe num (split' num)

-- Returns Just if value is changed, Nothing otherwise
split' :: Number -> Maybe Number
split' (Regular x) = if x >= 10
    then Just $ Pair (Regular $ div x 2) (Regular $ (div x 2) + (mod x 2))
    else Nothing
split' (Pair l r)  = case split' l of
    (Just num) -> Just $ Pair num r
    Nothing    -> case split' r of
        (Just num) -> Just $ Pair l num
        Nothing    -> Nothing

add :: Number -> Number -> Number
add l r = reduce (Pair l r)

pairs vals = [(x,y) | x <- vals, y <- vals, x /= y]

magnitude :: Number -> Int
magnitude (Regular x) = x
magnitude (Pair l r)  = (3 * magnitude l) + (2 * magnitude r)

partOne = magnitude . foldl1 add
partTwo = maximum . map (magnitude . uncurry add) . pairs

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . must . parse inputP ""
