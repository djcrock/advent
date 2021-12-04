import Data.List ( (\\), inits, intersect, transpose )
import Text.Parsec

type Parser a = Parsec String () a
type Moves = [Int]
type Row = [Int]
type Board = [Row]

natP :: Parser Int
natP = read <$> many1 digit

movesP :: Parser Moves
movesP = sepBy natP (char ',')

rowP :: Parser Row
rowP = (many $ char ' ') *> sepBy1 natP (many1 $ char ' ')

boardP :: Parser Board
boardP = sepEndBy1 rowP newline

inputParser :: Parser (Moves, [Board])
inputParser = do
    moves <- movesP
    newline
    newline
    boards <- sepEndBy1 boardP newline
    pure (moves, boards)

must (Right x) = x
must (Left y)  = error $ show y

isRowWinner :: Moves -> Row -> Bool
isRowWinner moves row = intersect row moves == row

isBoardWinner :: Moves -> Board -> Bool
isBoardWinner moves board = any (isRowWinner moves) (board ++ transpose board)

getWinners :: Moves -> Moves -> [Board] -> [(Moves, Board)]
getWinners _ [] _ = []
getWinners prevMoves (move:nextMoves) boards = map (moves,) winners ++ nextWinners
    where moves       = move:prevMoves
          winners     = filter (isBoardWinner moves) boards
          nextWinners = getWinners moves nextMoves (boards \\ winners)

score :: (Moves, Board) -> Int
score (moves, board) = (head moves) * (sum unmarked)
    where unmarked = (concat board) \\ moves

partOne (moves, boards) = score $ head $ getWinners [] moves boards
partTwo (moves, boards) = score $ last $ getWinners [] moves boards

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . must . parse inputParser ""
