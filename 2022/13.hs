import Text.Parsec

data Packet = L [Packet] | V Int deriving Eq

parseInput :: String -> [[Packet]]
parseInput = must . parse (sepBy pairP newline) ""
    where must    = either (error . show) id
          pairP   = sepEndBy packetP newline
          packetP = try listP <|> valP
          listP   = L <$> (char '[' *> sepBy packetP (char ',') <* char ']')
          valP    = V . read <$> many1 digit
          
instance Ord Packet where
    compare (L [])     (L [])     = EQ
    compare (L [])     _          = LT
    compare _          (L [])     = GT
    compare (V a)      (V b)      = compare a b
    compare a@(V _)    b@(L _)    = compare (L [a]) b
    compare a@(L _)    b@(V _)    = compare a (L [b])
    compare (L (a:as)) (L (b:bs)) = compare a b <> compare (L as) (L bs)

partOne = sum . map fst . filter (\(_,[xs,ys]) -> xs < ys) . zip [1..]
partTwo = product . map length . (sequence (filter<$>(>=)<$>[V 2,V 6])) . concat . ([V 2,V 6]:)

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parseInput
