import Advent
import qualified Data.Map as Map

type Robot = (Vec2,Vec2)
type Canvas = Map.Map Vec2 Bool

initialRobot = ((0,0),(0,1))

readCanvas :: Vec2 -> Canvas -> Bool
readCanvas pos = Map.findWithDefault False pos

paintCanvas :: Robot -> Bool -> Canvas -> Canvas
paintCanvas (pos,_) color = Map.insert pos color

move :: Bool -> Robot -> Robot
move turnRight (pos,(dirX,dirY)) = (vec2Add pos newDir, newDir)
    where newDir = if turnRight then (dirY,-dirX) else (-dirY,dirX)

runRobot :: Robot -> Canvas -> Computer -> Canvas
runRobot r@(pos,_) canvas comp = if hasOutput comp'
                            then runRobot r' canvas' comp'''
                            else canvas
    where comp' = run $ pushInput (fromEnum $ readCanvas pos canvas) comp
          (Just color,comp'') = popOutput comp'
          (Just turnRight,comp''') = popOutput comp''
          canvas' = paintCanvas r (toEnum color) canvas
          r' = move (toEnum turnRight) r

plot :: Canvas -> String
plot canvas = unlines
    [ [ if readCanvas (x,y) canvas then '#' else ' '
    | x <- [minX..maxX] ]
    | y <- [maxY,maxY-1..minY] ]
    where minX = minimum $ map fst $ Map.keys canvas
          maxX = maximum $ map fst $ Map.keys canvas
          minY = minimum $ map snd $ Map.keys canvas
          maxY = maximum $ map snd $ Map.keys canvas

partOne = show . length . Map.keys . runRobot initialRobot Map.empty
partTwo = plot . runRobot initialRobot (Map.singleton (0,0) True)

main = runSolutionsStr [partOne, partTwo] readComputer
