import qualified Data.Map as Map

-- (players@[(position, score)], rolls)
type Game = ([(Int,Int)],Int)

parseInput :: String -> Game
parseInput = (,0) . map ((,0) . read . drop 28) . lines

makeMove :: Int -> Game -> Game
makeMove move (players, rolls) = (players', rolls+3)
    where turn        = div (mod rolls (3 * length players)) (length players)
          (pos,score) = players !! turn
          pos'        = (mod (pos + move - 1) 10) + 1
          player'     = (pos', score + pos')
          players'    = (take turn players) ++ player' : (drop (turn+1) players)

wins :: Int -> Game -> [Int]
wins threshold = map (fromEnum . (>= threshold) . snd) . fst

practice :: Game -> Game
practice game@(_,rolls)
    | any (>0) (wins 1000 game) = game
    | otherwise = practice (makeMove move game)
    where move = sum [(mod x 100) + 1 | x <- [rolls..rolls+2]]
          
dirac :: Map.Map Game [Int] -> Game -> (Map.Map Game [Int],[Int])
dirac memo game
    | Map.member game memo  = (memo, memo Map.! game)
    | any (>0) winners      = (Map.insert game winners memo, winners)
    | otherwise             = foldr play (memo, [0,0..]) diracRolls
    where winners    = wins 21 game
          diracRolls = [(3,1),(4,3),(5,6),(6,7),(7,6),(8,3),(9,1)]
          play (move, universes) (memo, totalWins) =
            let (memo', wins) = (dirac memo $ makeMove move game)
                totalWins'    = zipWith (+) totalWins (map (* universes) wins)
            in  (Map.insert game totalWins' memo', totalWins')

partOne = (\(players, rolls) -> (minimum $ map snd players) * rolls) . practice
partTwo = maximum . snd . dirac Map.empty

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parseInput
