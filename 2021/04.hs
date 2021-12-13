import Data.List ( (\\), intersect, transpose )
import Text.Parsec

type Parser a = Parsec String () a
type Moves = [Int]
type Board = [[Int]]

inputP :: Parser (Moves, [Board])
inputP = (,) <$> (movesP <* newline <* newline) <*> boardsP
    where movesP = sepBy natP (char ',')
          boardsP = sepEndBy1 boardP newline
          boardP = sepEndBy1 rowP newline
          rowP = (many $ char ' ') *> sepBy1 natP (many1 $ char ' ')
          natP = read <$> many1 digit

must (Right x) = x
must (Left y)  = error $ show y

isBoardWinner :: Moves -> Board -> Bool
isBoardWinner moves board = any isRowWinner (board ++ transpose board)
    where isRowWinner row = intersect row moves == row

getWinners :: Moves -> Moves -> [Board] -> [(Moves, Board)]
getWinners _ [] _ = []
getWinners prevMoves (move:nextMoves) boards =
    (map (moves,) winners) ++ nextWinners
        where moves       = move:prevMoves
              winners     = filter (isBoardWinner moves) boards
              nextWinners = getWinners moves nextMoves (boards \\ winners)

score :: (Moves, Board) -> Int
score (moves, board) = (head moves) * (sum unmarked)
    where unmarked = (concat board) \\ moves

partOne = score . head . uncurry (getWinners [])
partTwo = score . last . uncurry (getWinners [])

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . must . parse inputP ""
