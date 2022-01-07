import Advent
import Text.Parsec
import qualified Data.Map as Map

type Species = String
type Term = (Int,Species)
type Equation = ([Term],Term)
type Components = Map.Map Species (Int,[Term])
type Excess = Map.Map Species Int

parseInput :: Parser [Equation]
parseInput = must . parse (sepEndBy eqP newline) ""
    where eqP   = (,) <$> (sepBy termP (char ',') <* string " =>") <*> termP
          termP = (,) <$> (spaces *> numP <* space) <*> many1 upper
          numP  = read <$> many1 digit
          must  = either (error . show) id

eqsToMap :: [Equation] -> Components
eqsToMap [] = Map.empty
eqsToMap ((terms,(n,prod)):xs) = Map.insert prod (n,terms) (eqsToMap xs)

consumeExcess :: Term -> Excess -> (Excess,Int)
consumeExcess (n,prod) excess = (Map.filter (/= 0) excess', n - consumed)
    where available = Map.findWithDefault 0 prod excess
          consumed  = min n available
          excess'   = Map.insert prod (available - consumed) excess

decompose :: Term -> Components -> (Excess,Int) -> (Excess,Int)
decompose (n,prod) comp (excess,total)
    | not (Map.member prod comp) = (excess,total+n)
    | otherwise = foldr decompTerm (excess'',total) terms
    where (batchSize,terms) = comp Map.! prod
          (excess',n')      = consumeExcess (n,prod) excess
          batches           = (div n' batchSize) + signum (mod n' batchSize)
          excess''          = Map.insertWith (+)
                                prod ((batches * batchSize) - n')
                                excess'
          decompTerm (tn,tprod) acc = decompose (batches*tn,tprod) comp acc

naiveCost :: Term -> Components -> Int
naiveCost term comp = snd $ decompose term comp (Map.empty,0)

maximizeOutput :: Species -> Int -> Components -> (Int,Int) -> (Int,Int)
maximizeOutput species ore comp (prevProduced,prevCost) = (toProduce,cost)
    where singleCost = naiveCost (1,species) comp
          toProduce  = prevProduced + div (ore - prevCost) singleCost
          cost       = naiveCost (toProduce,species) comp
          

partOne = naiveCost (1,"FUEL")
partTwo comp = fst $ fix (maximizeOutput "FUEL" 1000000000000 comp) (0,0)

main = runSolutions [partOne, partTwo] (eqsToMap . parseInput)
