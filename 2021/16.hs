import Control.Applicative
import Data.Bits ( testBit )
import Data.Char ( digitToInt )

type Bits = [Bool]
type Version = Int
data Packet = Literal Version Int
            | Operator Version Int [Packet]
            deriving (Show)

-- Shout-out to Programming in Haskell 2nd Edition chapter 13 :D
-- I probably cound have used Text.Parsec, but this was fun
newtype Parser a = P (Bits -> [(a,Bits)])
parse :: Parser a -> Bits -> [(a,Bits)]
parse (P p) = p
instance Functor Parser where
    fmap g p = P (\inp -> case parse p inp of
                    []        -> []
                    [(v,out)] -> [(g v, out)])
instance Applicative Parser where
    pure v    = P (\inp -> [(v,inp)])
    pg <*> px = P (\inp -> case parse pg inp of
                    []        -> []
                    [(g,out)] -> parse (fmap g px) out)
instance Monad Parser where
    p >>= f = P (\inp -> case parse p inp of
                    []        -> []
                    [(v,out)] -> parse (f v) out)
instance Alternative Parser where
    empty   = P (\inp -> [])
    p <|> q = P (\inp -> case parse p inp of
                []        -> parse q inp
                [(v,out)] -> [(v,out)])

toBits :: String -> Bits
toBits = concatMap hexToBits . head . lines

toPacket :: String -> Packet
toPacket = fst . head . parse packet . toBits

hexToBits :: Char -> Bits
hexToBits c = map (testBit $ digitToInt c) [3,2,1,0]

bitsToInt :: Bits -> Int
bitsToInt = foldl (\int bit -> int * 2 + fromEnum bit) 0

bit :: Parser Bool
bit = P (\inp -> case inp of
            []     -> []
            (x:xs) -> [(x,xs)])

bits :: Int -> Parser Bits
bits n | n <= 0    = pure []
       | otherwise = sequence (replicate n bit)

bitOf :: Bool -> Parser Bool
bitOf b = do
    x <- bit
    if x == b then pure x else empty

one = bitOf True
zero = bitOf False

anyNum :: Int -> Parser Int
anyNum digits = bitsToInt <$> bits digits

num :: Int -> Int -> Parser Int
num digits val = do
    x <- anyNum digits
    if x == val then pure x else empty

version = anyNum 3
typeId = num 3

literal :: Parser Packet
literal = Literal <$> (version <* typeId 4) <*> (bitsToInt <$> payload)
    where payload = (++)
            <$> (concat <$> many (one *> bits 4))
            <*> (zero *> bits 4)

operator :: Parser Packet
operator = do
    v <- version
    tid <- anyNum 3
    lengthTypeId <- bit
    amount <- if lengthTypeId then anyNum 11 else anyNum 15
    ps <- if lengthTypeId
            then packetsByCount amount
            else packetsByLength amount
    pure $ Operator v tid ps

packet :: Parser Packet
packet = literal <|> operator

packetsByCount :: Int -> Parser [Packet]
packetsByCount n | n <= 0    = pure []
                 | otherwise = sequence (replicate n packet)

packetsByLength :: Int -> Parser [Packet]
packetsByLength n | n <= 0    = pure []
                  | otherwise = do
                        bs <- bits n
                        let [(ps,_)] = parse (many packet) bs
                        pure ps

versionSum :: Packet -> Int
versionSum (Literal v _)     = v
versionSum (Operator v _ ps) = v + sum (map versionSum ps)

eval :: Packet -> Int
eval (Literal _ x) = x
eval (Operator _ tid ps) = case tid of
    0 -> sum vals
    1 -> product vals
    2 -> minimum vals
    3 -> maximum vals
    5 -> if head vals >  last vals then 1 else 0
    6 -> if head vals <  last vals then 1 else 0
    7 -> if head vals == last vals then 1 else 0
    _ -> error "Unknown packet type"
    where vals = map eval ps

partOne = versionSum
partTwo = eval

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . toPacket
