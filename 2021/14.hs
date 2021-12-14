import Data.List ( nub )
import Data.Maybe
import Text.Parsec
import qualified Data.Map as Map

type Parser a = Parsec String () a
type Polymer = String
type Rules = Map.Map (Char, Char) Char
type Freqs = Map.Map Char Int
type Memo = Map.Map ((Char, Char, Int)) Freqs

inputP :: Parser (Polymer, Rules)
inputP = (,) <$> (many1 upper <* newline <* newline) <*> rulesP
    where rulesP = Map.fromList <$> sepEndBy1 ruleP newline
          ruleP  = (,) <$> (pairP <* string " -> ") <*> upper
          pairP  = (,) <$> upper <*> upper

must (Right x) = x
must (Left y)  = error $ show y

toFreqs :: Polymer -> Freqs
toFreqs poly = Map.fromList [(x, length $ filter (==x) poly) | x <- nub poly]

freqsAt :: Rules -> Memo -> Polymer -> Int -> (Memo, Freqs)
-- Avoid double-counting the last element
-- It is already counted as the first element of the RHS of the recursion
-- The only exception to this is the very last element of the original polymer
-- It is added back in the "scoreAt" function
freqsAt _ memo poly 0 = (memo, toFreqs $ init poly)
freqsAt _ memo [x] _ = (memo, Map.empty)
freqsAt rules memo poly@(x:y:rest) depth = 
    (memo''', Map.unionWith (+) freqs freqsRest)
    where between = rules Map.! (x,y)
          memoVal = Map.lookup (x,y,depth) memo
          (memo', freqs) = if isJust memoVal
              then (memo, fromJust memoVal)
              else freqsAt rules memo (x:between:[y]) (depth-1)
          memo'' = Map.insert (x,y,depth) freqs memo'
          (memo''', freqsRest) = freqsAt rules memo'' (y:rest) depth

scoreAt :: Int -> (Polymer, Rules) -> Int
scoreAt depth (poly, rules) = (maximum counts) - (minimum counts)
    where (_, freqs) = freqsAt rules Map.empty poly depth
          -- Add the last element back in
          fixedFreqs = Map.insertWith (+) (last poly) 1 freqs
          counts = Map.elems fixedFreqs

partOne = scoreAt 10
partTwo = scoreAt 40

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . must . parse inputP ""
