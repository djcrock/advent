module Advent where

import Control.Applicative
import Control.Monad
import Control.Monad.State hiding ( fix )
import Data.List
import Data.Ord ( comparing )
import qualified Data.Map as Map

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

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn c (x:xs)
    | x == c    = "" : segment : segments
    | otherwise = (x:segment) : segments
    where (segment:segments) = splitOn c xs

disjoint :: Eq a => [a] -> [a] -> [a]
disjoint xs ys = union xs ys \\ intersect xs ys

deleteAll :: Eq a => a -> [a] -> [a]
deleteAll _ []     = []
deleteAll x (y:ys) = (if x == y then id else (y:)) $ deleteAll x ys

categorize :: Eq b => (a -> b) -> [a] -> [(b,[a])]
categorize _ []     = []
categorize f (x:xs) = (f x, matches) : categorize f rest
    where (matches,rest) = partition ((==(f x)) . f) (x:xs)

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn = minimumBy . comparing

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn = maximumBy . comparing

-- Intcode

type Address = Int
type Memory = Map.Map Address Int
type Mutation = State Computer ()
type Stateful a = State Computer a
data Computer = Computer
    { instPtr :: Address
    , relBase :: Address
    , memory  :: Memory
    , inputs  :: Queue Int
    , outputs :: Queue Int
    } deriving (Eq, Show)

readMemory :: Parser Memory
readMemory = Map.fromList . zip [0..] . readCommaSep

readComputer :: Parser Computer
readComputer str = let intCode = readMemory str in Computer
    { instPtr = 0
    , relBase = 0
    , memory  = intCode
    , inputs  = emptyQ
    , outputs = emptyQ
    }

getPtr :: Stateful Address
getPtr = gets instPtr

incPtr :: Int -> Mutation
incPtr = offset getPtr setPtr

setPtr :: Address -> Mutation
setPtr addr = modify $ \c -> c { instPtr = addr }

getRelBase :: Stateful Address
getRelBase = gets relBase

incRelBase :: Int -> Mutation
incRelBase = offset getRelBase setRelBase

setRelBase :: Address -> Mutation
setRelBase addr = modify $ \c -> c { relBase = addr }

getAddr :: Address -> Stateful Int
getAddr addr = gets $ (maybe 0 id . (Map.lookup addr) . memory)

deref :: Address -> Stateful Int
deref = getAddr <=< getAddr

setAddr :: Address -> Int -> Mutation
setAddr addr val = modify $ \c -> c { memory = Map.insert addr val (memory c) }

offset :: Stateful Address -> (Address -> Stateful a) -> Int -> Stateful a
offset sa f n = sa >>= f . (+n)

param :: Int -> Stateful Int
param n = do
    opcode <- decodeOp
    case opcode !! n of
        0 -> offset getPtr deref   n
        1 -> offset getPtr getAddr n
        2 -> offset getPtr getAddr n >>= offset getRelBase getAddr

outputParam :: Int -> Int -> Mutation
outputParam n val = do
    opcode <- decodeOp
    case opcode !! n of
        0 -> offset getPtr getAddr n >>= flip setAddr val
        1 -> crash "Output parameters cannot be in immediate mode"
        2 -> offset getPtr getAddr n >>= offset getRelBase (flip setAddr val)

binaryOp :: Enum a => (Int -> Int -> a) -> Mutation
binaryOp f = do
    x <- param 1
    y <- param 2
    -- fromEnum handles both Ints and Bools (True/False -> 1/0)
    outputParam 3 (fromEnum $ f x y)
    incPtr 4

-- Read from the input buffer. Blocks on an empty buffer.
opInput :: Mutation
opInput = do
    val <- gets inputs
    case popQ val of
        (Nothing,_)   -> pure ()
        (Just x,val') -> do
            modify $ \c -> c { inputs = val' }
            outputParam 1 x
            incPtr 2

-- Write to the output buffer
opOutput :: Mutation
opOutput = do
    val <- param 1
    modify $ \c -> c { outputs = pushQ val (outputs c) }
    incPtr 2

opJumpIf :: (Int -> Bool) -> Mutation
opJumpIf pred = do
    val <- param 1
    tgt <- param 2
    if pred val
        then setPtr tgt
        else incPtr 3

-- Decode the opcode into a stack of parameter modes with the operation on top.
-- Infinite 0s are appended to account for any number of parameters.
-- e.g. 1002 -> [2,0,1,0,0..] (note: the operation here is "02")
decodeOp :: Stateful [Int]
decodeOp = do
    opcode <- offset getPtr getAddr 0
    let digits = (reverse (intToDigits opcode)) ++ repeat 0
    let operation = digitsToInt (reverse (take 2 digits))
    pure (operation : drop 2 digits)

opUpdateRelBase :: Mutation
opUpdateRelBase = (param 1 >>= incRelBase) *> incPtr 2

halt :: Mutation
halt = pure ()

crash :: String -> Mutation
crash msg = do
    c <- get
    error $ msg ++ " --- " ++ show c

step :: Mutation
step = do
    opcode <- decodeOp
    case head opcode of
        1  -> binaryOp (+)
        2  -> binaryOp (*)
        3  -> opInput
        4  -> opOutput
        5  -> opJumpIf (/= 0)
        6  -> opJumpIf (== 0)
        7  -> binaryOp (<)
        8  -> binaryOp (==)
        9  -> opUpdateRelBase
        99 -> halt
        otherwise -> crash "Unexpected op"

run :: Computer -> Computer
run = fix (execState step)

peek :: Address -> Computer -> Int
peek n = maybe 0 id . (Map.lookup n) . memory

poke :: Address -> Int -> Computer -> Computer
poke addr val c = c { memory = Map.insert addr val (memory c) }

setInputs :: [Int] -> Computer -> Computer
setInputs xs c = c { inputs = fromListQ xs }

pushInput :: Int -> Computer -> Computer
pushInput x c = c { inputs = pushQ x (inputs c) }

popOutput :: Computer -> (Maybe Int, Computer)
popOutput c = (out, c { outputs = outs })
    where (out,outs) = popQ (outputs c)

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

digitsToInt :: [Int] -> Int
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
