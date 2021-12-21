import qualified Data.Map as Map

type State = ((Int,Int), (Int,Int), Int)

parseInput :: String -> (Int,Int)
parseInput = (\[p1,p2] -> (p1,p2)) . map (read . drop 28) . lines

playDeterministic :: State -> State
playDeterministic (positions, scores, rolls) =
    (set position positions, set score scores, rolls+3)
    where isPlayerOne   = mod rolls 6 == 0
          getActive     = if isPlayerOne then fst else snd
          set x (p1,p2) = if isPlayerOne then (x,p2) else (p1,x)
          move          = sum [(mod x 100) + 1 | x <- [rolls..rolls+2]]
          position      = (mod ((getActive positions) + move - 1) 10) + 1
          score         = (getActive scores) + position
          
diracMap :: Map.Map Int Int
diracMap = foldr (\k -> Map.insertWith (+) k 1) Map.empty possibleRolls
    where possibleRolls = [x+y+z | x <- [1..3], y <- [1..3], z <- [1..3]]

playDirac :: State -> (Int, Int)
playDirac (positions, scores@(s1,s2), rolls)
    | s1 >= 21 = (1,0)
    | s2 >= 21 = (0,1)
    | otherwise  = foldr1 (\(w1,w2) (w3,w4) -> (w1+w3, w2+w4)) $ do
        roll <- [3..9]
        let universes = diracMap Map.! roll
        let position  = (mod ((getActive positions) + roll - 1) 10) + 1
        let score     = (getActive scores) + position
        let newState  = (set position positions, set score scores, rolls+3)
        let (w1,w2)   = playDirac newState
        pure (w1*universes, w2*universes)
    where isPlayerOne   = mod rolls 6 == 0
          getActive     = if isPlayerOne then fst else snd
          set x (p1,p2) = if isPlayerOne then (x,p2) else (p1,x)


won :: State -> Bool
won (_, (s1,s2), _) = s1 >= 1000 || s2 >= 1000

solution :: State -> Int
solution (_, (s1,s2), rolls) = (min s1 s2) * rolls

partOne positions = solution $ head $ dropWhile (not . won) $ iterate playDeterministic (positions, (0,0), 0)
--partOne positions = take 3 $ iterate play (positions, (0,0), 0)
partTwo positions = uncurry max $ playDirac (positions, (0,0), 0)

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parseInput
