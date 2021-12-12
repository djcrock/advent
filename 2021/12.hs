import Data.Char ( isUpper )
import qualified Data.Map as Map

data Node = Start
          | Large String
          | Small String
          | End
          deriving (Eq, Ord, Show)
type Graph = Map.Map Node [Node]
type Path = [Node]

parse :: String -> Graph
parse = toGraph . map (sort . parseEdge) . lines
    where parseEdge str =
            ( toNode $ takeWhile (/='-') str
            , toNode $ tail $ dropWhile (/='-') str )
          toNode str
            | str == "start"     = Start
            | str == "end"       = End
            | isUpper (head str) = Large str
            | otherwise          = Small str
          sort (x, Start) = (Start, x)
          sort (End, x)   = (x, End)
          sort x          = x
          toGraph = foldr addEdge Map.empty
          addEdge (Start, to) = graphInsert Start to
          addEdge (from, End) = graphInsert from End
          addEdge (from, to)  = (graphInsert to from) . (graphInsert from to)
          graphInsert from to = Map.insertWith (++) from [to]

explore :: Bool -> Graph -> Path -> [Path]
explore canRevisit graph path@(current:prev)
    | current == End = [path]
    | isRevisit && not canRevisit = []
    | otherwise = concatMap ((explore canRevisitNext graph) . (:path)) next
    where next = graph Map.! current
          isRevisit = isSmall current && elem current prev
          canRevisitNext = canRevisit && not isRevisit
          isSmall (Small _) = True
          isSmall _         = False

partOne = length . (\g -> explore False g [Start])
partTwo = length . (\g -> explore True  g [Start])

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
