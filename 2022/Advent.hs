module Advent where

import Data.List
import Data.Ord ( comparing )

runSolutions :: Show b => [(a -> b)] -> (String -> a) -> IO ()
runSolutions ss parser = interact $ (++ "\n") . show . sequence ss . parser

runSolutionsStr :: [(a -> String)] -> (String -> a) -> IO ()
runSolutionsStr ss parser = interact $ unlines . sequence ss . parser

-- Parsers

type Parser a = (String -> a)

readLines :: Read a => Parser [a]
readLines = map read . lines

readCommaSep :: Read a => Parser [a]
readCommaSep = map read . splitOn ','

-- Lists

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn c (x:xs)
    | x == c    = [] : segment : segments
    | otherwise = (x:segment) : segments
    where (segment:segments) = splitOn c xs

splitOnList :: Eq a => [a] -> [a] -> [[a]]
splitOnList [] _ = error "Tried to split on empty list"
splitOnList _ [] = [[]]
splitOnList xs (y:ys)
    | isPrefixOf xs (y:ys) = [] : segment' : segments'
    | otherwise            = (y:segment) : segments
    where (segment:segments)   = splitOnList xs ys
          (segment':segments') = splitOnList xs (drop (length xs) (y:ys))

-- Split a list into segments of a given length
segment :: Int -> [a] -> [[a]]
segment n xs = case splitAt n xs of
    (curr, [])   -> [curr]
    (curr, rest) -> curr : segment n rest

disjoint :: Eq a => [a] -> [a] -> [a]
disjoint xs ys = union xs ys \\ intersect xs ys

deleteAll :: Eq a => a -> [a] -> [a]
deleteAll _ []     = []
deleteAll x (y:ys) = (if x == y then id else (y:)) $ deleteAll x ys

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

categorize :: Eq b => (a -> b) -> [a] -> [(b,[a])]
categorize _ []     = []
categorize f (x:xs) = (f x, matches) : categorize f rest
    where (matches,rest) = partition ((==(f x)) . f) (x:xs)

chunksOf :: Eq a => [[a]] -> [a] -> Maybe [[a]]
chunksOf _ []  = pure []
chunksOf xs ys = do
    prefix <- find (flip isPrefixOf ys) xs
    rest   <- chunksOf xs $ drop (length prefix) ys
    pure (prefix : rest)

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn = minimumBy . comparing

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn = maximumBy . comparing

-- Queues

data Queue a = Queue [a] [a] deriving (Eq, Show)
--newtype Queue a = Queue [a] deriving (Eq, Show)

emptyQ :: Queue a
emptyQ = Queue [] []

pushQ :: a -> Queue a -> Queue a
pushQ x (Queue ins outs) = Queue (x:ins) outs

popQ :: Queue a -> (Maybe a, Queue a)
popQ (Queue []  []        ) = (Nothing,  Queue []  []  )
popQ (Queue ins (out:outs)) = (Just out, Queue ins outs)
popQ (Queue ins []        ) = (Just out, Queue []  outs)
    where (out:outs) = reverse ins

fromListQ :: [a] -> Queue a
fromListQ = Queue []

toListQ :: Queue a -> [a]
toListQ (Queue ins outs) = outs ++ reverse ins

-- Helpful functions

-- Iterate a function until the result stops changing, returning the final value
fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == f x then x else fix f (f x)

-- Iterate a function until the result stops changing, returning each value
iterateFix :: Eq a => (a -> a) -> a -> [a]
iterateFix f x = if x == f x then [x] else x : iterateFix f (f x)

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

intToDigits :: Int -> [Int]
intToDigits 0 = []
intToDigits n = intToDigits (div n 10) ++ [mod n 10]

digitsToInt :: Integral a => [a] -> a
digitsToInt = foldl1 (\acc digit -> acc * 10 + digit)

-- Get the length of each run of matching list elements
-- e.g. "foobar" -> [1,2,1,1,1]
runLengths :: Eq a => [a] -> [Int]
runLengths []       = []
runLengths xs@(x:_) = length same : runLengths rest
    where (same,rest) = span (== x) xs

-- Vectors

type Vec2 = (Int,Int)

vec2Op :: (Int -> Int -> Int) -> Vec2 -> Vec2 -> Vec2
vec2Op f (x1,y1) (x2,y2) = (f x1 x2, f y1 y2)

vec2Add :: Vec2 -> Vec2 -> Vec2
vec2Add = vec2Op (+)

manhattan :: Vec2 -> Vec2 -> Int
manhattan (x1,y1) (x2,y2) = (abs (x2-x1)) + (abs (y2-y1))

orthoAdjacent :: Vec2 -> [Vec2]
orthoAdjacent (x,y) =
    [ (x  , y+1)
    , (x+1, y  )
    , (x  , y-1)
    , (x-1, y  ) ]
