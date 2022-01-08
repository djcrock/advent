import Advent
import Control.Monad.State
import Data.List ( sortOn )
import Data.Maybe ( fromJust, listToMaybe, mapMaybe )
import qualified Data.Set as Set

data Direction = North | South | West | East deriving (Enum, Eq)
data Tile = Wall | Empty | Goal deriving (Enum, Eq)
type Path = [Vec2]

moveCoord :: Direction -> Vec2 -> Vec2
moveCoord North = vec2Add ( 0, 1)
moveCoord South = vec2Add ( 0,-1)
moveCoord West  = vec2Add (-1, 0)
moveCoord East  = vec2Add ( 1, 0)

tryMove :: Path -> Direction -> Stateful (Path,Tile)
tryMove path@(pos:_) dir = do
    let path' = (moveCoord dir pos) : path
    pushInputS ((fromEnum dir) + 1)
    go
    tile <- toEnum <$> mustPopOutputS
    pure (path',tile)

findGoal :: ((Path,Tile),Computer) -> Maybe Path
findGoal ((path@(pos:prev),tile),c) = case tile of
    Wall  -> Nothing
    Goal  -> Just path
    Empty -> if elem pos prev then Nothing else
        listToMaybe $ sortOn length next
    where next = mapMaybe (findGoal . flip runState c . tryMove path) [North .. East]

explore :: ((Path,Tile),Computer) -> Set.Set Vec2
explore ((path@(pos:prev),tile),c) = case tile of
    Wall -> Set.empty
    _    -> if elem pos prev then Set.empty else
        foldr Set.union (Set.singleton pos) next
    where next = map (explore . flip runState c . tryMove path) [North .. East]

oxygenTick :: (Set.Set Vec2, Set.Set Vec2) -> (Set.Set Vec2, Set.Set Vec2)
oxygenTick (front,unvisited) = Set.partition (`Set.member` next) unvisited
    where next = Set.fromList $ concatMap (orthoAdjacent) $ Set.elems front

startingPosition = ([(0,0)],Empty)

partOne = length . tail . fromJust . findGoal . (startingPosition,)
partTwo c = length $ takeWhile ((>0) . Set.size . snd) $ ticks
    where oxygenLoc  = head $ fromJust $ findGoal (startingPosition,c)
          emptyTiles = Set.delete oxygenLoc $ explore (startingPosition,c)
          ticks      = iterate oxygenTick (Set.singleton oxygenLoc, emptyTiles)

main = runSolutions [partOne, partTwo] readComputer
