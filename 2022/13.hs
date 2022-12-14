import Data.List ( findIndices, sort )
import Text.Parsec

data Packet = List [Packet] | Val Int deriving ( Eq )

parseInput :: String -> [[Packet]]
parseInput = must . parse (sepBy pairP newline) ""
    where must    = either (error . show) id
          pairP   = sepEndBy packetP newline
          packetP = try listP <|> valP
          listP   = List <$> (char '[' *> sepBy packetP (char ',') <* char ']')
          valP    = Val . read <$> many1 digit
          
instance Ord Packet where
    compare (List [])     (List (_:_))  = LT
    compare (List (_:_))  (List [])     = GT
    compare (List [])     (List [])     = EQ
    compare (Val a)       (Val b)       = compare a b
    compare a@(Val _)     b@(List _)    = compare (List [a]) b
    compare a@(List _)    b@(Val _)     = compare a (List [b])
    compare (List (a:as)) (List (b:bs)) = case compare a b of
                                              EQ -> compare (List as) (List bs)
                                              x  -> x

partOne = sum . map fst . filter (isSorted . snd) . zip [1..]
    where isSorted xs = xs == sort xs

partTwo = product . indices dividers . sort . (dividers ++) . concat
    where dividers   = [List [List [Val 2]], List [List [Val 6]]]
          indices xs = map (+1) . findIndices (`elem` xs)

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parseInput
