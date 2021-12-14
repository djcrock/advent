import Data.List ( nub )
import Text.Parsec
import qualified Data.Map as Map

type Parser a = Parsec String () a
type Polymer = String
type Rules = Map.Map (Char, Char) Char

inputP :: Parser (Polymer, Rules)
inputP = (,) <$> (many1 upper <* newline <* newline) <*> rulesP
    where rulesP = Map.fromList <$> sepEndBy1 ruleP newline
          ruleP  = (,) <$> (pairP <* string " -> ") <*> upper
          pairP  = (,) <$> upper <*> upper

must (Right x) = x
must (Left y)  = error $ show y

react :: Rules -> Polymer -> Polymer
react rules (x:y:rest) = x : (rules Map.! (x,y)) : (react rules (y:rest))
react _ [x] = [x]

score :: Polymer -> Int
score poly = (maximum freqs) - (minimum freqs)
    where freqs = [length $ filter (==x) poly | x <- nub poly]

partOne (poly, rules) = score $ (!! 10) $ iterate (react rules) poly
partTwo = partOne

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . must . parse inputP ""
