import Data.Either ( fromRight, lefts, rights )
import Data.List ( sort )
import Text.Parsec

type Parser a = Parsec String () a
data Tree = Dir String [Tree]
          | File Int String deriving (Show)

fileP :: Parser Tree
fileP = File <$> ((read <$> many1 digit) <* space) <*> many1 (noneOf "\n")

dirP :: Parser Tree
dirP = Dir <$> startP <*> (lsP *> sepEndBy treeP newline <* endP)
    where startP = (string "$ cd " *> nameP <* newline)
          nameP  = try (lookAhead (noneOf ".")) *> many (noneOf "\n")
          lsP    = string "$ ls" *> newline
          endP   = option "" (string "$ cd ..")

treeP :: Parser Tree
treeP = sepEndBy dirNameP newline *> try (dirP <|> fileP)
    where dirNameP = string "dir " *> many1 (noneOf "\n")

rootP :: Parser Tree
rootP = Dir "/" <$> (string "$ cd /\n$ ls\n" *> sepEndBy treeP newline <* eof)

parseInput :: String -> Tree
parseInput = must . parse rootP ""
    where must = either (error . show) id

treeSizes :: Tree -> [Either Int Int]
treeSizes (File n _) = [Left n]
treeSizes (Dir _ xs) = (Right $ sum $ lefts sizes) : sizes
    where sizes = concatMap treeSizes xs

partOne      = sum . filter (<=100000) . rights . treeSizes
partTwo tree = head $ sort $ filter (>=target) $ rights sizes
    where sizes     = treeSizes tree
          totalUsed = fromRight 0 $ head sizes
          target    = 30000000 - (70000000 - totalUsed)

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parseInput
