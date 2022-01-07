import Advent
import Data.List ( find )
import qualified Data.Map as Map

data Tile = Empty | Wall | Block | Paddle | Ball deriving (Enum, Eq)
type Canvas = Map.Map Vec2 Tile
type Display = (Canvas, Int)

initDisp = (Map.empty,0)

readCanvas :: Vec2 -> Canvas -> Tile
readCanvas pos = Map.findWithDefault Empty pos

updateDisplay :: Display -> Computer -> (Display,Computer)
updateDisplay d c = (foldl setPix d (segment 3 outs), c')
    where (outs,c') = allOutput c
          setPix display [] = display
          setPix (canvas,score) [x,y,pix] =
            if (x,y) == (-1,0)
                then (canvas,pix)
                else (Map.insert (x,y) (toEnum pix) canvas, score)

isWinner :: Display -> Bool
isWinner (canvas,score) = score > 0 && (not $ elem Block $ Map.elems canvas)

bigBrainAI :: Display -> Int
bigBrainAI d = maybe 0 id $ do
    (px,_) <- findTile Paddle d
    (bx,_) <- findTile Ball d
    pure $ signum (bx - px)
    where findTile x = fmap fst . find ((== x) . snd) . Map.assocs . fst

tick :: (Display,Computer) -> (Display,Computer)
tick (d,c) = if isWinner d then (d,c) else (d', c''')
    where c'       = run c
          (d',c'') = updateDisplay d c'
          c'''     = setInputs [bigBrainAI d'] c''

plot :: Display -> String
plot (canvas,score) = "Score: " ++ show score ++ "\n" ++ unlines
    [ [ case readCanvas (x,y) canvas of
            Empty  -> ' '
            Wall   -> '#'
            Block  -> '='
            Paddle -> '_'
            Ball   -> '.'
    | x <- [0..maximum $ map fst $ Map.keys canvas] ]
    | y <- [0..maximum $ map snd $ Map.keys canvas] ]

partOne = show . Map.size . Map.filter (== Block) . fst . fst . tick . (initDisp,)
partTwo = show . snd . fst . fix tick . (initDisp,) . poke 0 2
-- Watch the game!
--partTwo = concatMap (plot . fst) . tail . iterateFix tick . (initDisp,) . poke 0 2

main = runSolutionsStr [partOne, partTwo] readComputer
