import Data.Char ( isLower )
import qualified Data.Map as Map

type Graph = Map.Map String [String]
type Path = [String]

parse :: String -> Graph
parse = toGraph . map parseEdge . lines
    where parseEdge str =
            ( takeWhile (/='-') str
            , tail $ dropWhile (/='-') str )
          toGraph = foldr addEdge Map.empty
          addEdge ("start", x) = graphInsert "start" x
          addEdge (x, "start") = graphInsert "start" x
          addEdge ("end", x)   = graphInsert x "end"
          addEdge (x, "end")   = graphInsert x "end"
          addEdge (x, y)       = (graphInsert x y) . (graphInsert y x)
          graphInsert from to = Map.insertWith (++) from [to]

explore :: Bool -> Graph -> Path -> [Path]
explore canRevisit graph path@(current:prev)
    | current == "end" = [path]
    | isRevisit && not canRevisit = []
    | otherwise = concatMap ((explore canRevisitNext graph) . (:path)) next
    where next = graph Map.! current
          isRevisit = isSmall && elem current prev
          canRevisitNext = canRevisit && not isRevisit
          isSmall = isLower (head current) && current /= "start"

partOne = length . (\g -> explore False g ["start"])
partTwo = length . (\g -> explore True  g ["start"])

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
