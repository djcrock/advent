import Data.List ( nub )
import Text.Parsec

type Parser a = Parsec String () a
type Point = (Int, Int)
data Fold = Vertical Int | Horizontal Int

inputP :: Parser ([Point], [Fold])
inputP = (,) <$> (pointsP <* newline) <*> foldsP
    where pointsP  = sepEndBy pointP newline
          pointP   = (,) <$> (natP <* char ',') <*> natP
          foldsP   = sepEndBy (try vFoldP <|> hFoldP) newline
          vFoldP   = Vertical   <$> (string "fold along x=" *> natP)
          hFoldP   = Horizontal <$> (string "fold along y=" *> natP)
          natP     = read <$> many1 digit

must (Right x) = x
must (Left y)  = error $ show y

foldDim :: Int -> Int -> Int
foldDim along val = if val < along then val else (along * 2) - val

foldSheet :: Fold -> [Point] -> [Point]
foldSheet (Vertical foldX)   = nub . map (\(x, y) -> (foldDim foldX x, y))
foldSheet (Horizontal foldY) = nub . map (\(x, y) -> (x, foldDim foldY y))

plot :: [Point] -> String
plot ps = unlines
    [ [ if elem (x,y) ps then '#' else ' '
        | x <- [0..maximum $ map fst ps] ]
        | y <- [0..maximum $ map snd ps] ]


partOne = show . length . (\(ps, (f:_)) -> foldSheet f ps)
partTwo = plot . uncurry (foldl (flip foldSheet))

main = interact $ unlines . sequence [partOne, partTwo] . must . parse inputP ""
