module Advent where

import Control.Applicative
import Control.Monad
import Control.Monad.State hiding ( fix )
import Data.Array
import Data.List

runSolutions :: Show b => [(a -> b)] -> (String -> a) -> IO ()
runSolutions ss parser = interact $ (++ "\n") . show . sequence ss . parser

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

-- Intcode

type Address = Int
type Memory = Array Address Int
type Mutation = State Computer ()
type Stateful a = State Computer a
data Computer = Computer
    { instPtr :: Address
    , memory  :: Memory
    , inputs  :: [Int]
    , outputs :: [Int]
    } deriving (Eq, Show)

readMemory :: Parser Memory
readMemory str = listArray (0, length ints - 1) ints
    where ints = readCommaSep str

readComputer :: Parser Computer
readComputer str = let intCode = readMemory str in Computer
    { instPtr = 0
    , memory  = intCode
    , inputs  = []
    , outputs = []
    }

getPtr :: State Computer Address
getPtr = gets instPtr

incPtr :: Int -> Mutation
incPtr = offset setPtr

setPtr :: Address -> Mutation
setPtr addr = modify $ \c -> c { instPtr = addr }

getAddr :: Address -> Stateful Int
getAddr addr = gets $ ((! addr) . memory)

deref :: Address -> Stateful Int
deref = getAddr <=< getAddr

setAddr :: Address -> Int -> Mutation
setAddr addr val = modify $ \c -> c { memory = memory c // [(addr,val)] }

offset :: (Address -> Stateful a) -> Int -> Stateful a
offset f n = gets instPtr >>= f . (+n)

param :: Int -> Stateful Int
param n = do
    opcode <- decodeOp
    case opcode !! n of
        0 -> offset deref   n
        1 -> offset getAddr n

outputParam :: Int -> Int -> Mutation
outputParam p val = offset getAddr p >>= flip setAddr val

binaryOp :: Enum a => (Int -> Int -> a) -> Mutation
binaryOp f = do
    x <- param 1
    y <- param 2
    -- fromEnum handles both Ints and Bools (True/False -> 1/0)
    outputParam 3 (fromEnum $ f x y)
    incPtr 4

opInput :: Mutation
opInput = do
    val <- gets $ head . inputs
    modify $ \c -> c { inputs = tail (inputs c) }
    outputParam 1 val
    incPtr 2

opOutput :: Mutation
opOutput = do
    val <- param 1
    modify $ \c -> c { outputs = val : outputs c }
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
    opcode <- offset getAddr 0
    let digits = (reverse (intToDigits opcode)) ++ repeat 0
    let operation = digitsToInt (reverse (take 2 digits))
    pure (operation : drop 2 digits)

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
        99 -> halt
        otherwise -> crash "Unexpected op"

run :: Computer -> Computer
run = fix (execState step)

peek :: Address -> Computer -> Int
peek n = (! n) . memory

poke :: Address -> Int -> Computer -> Computer
poke addr val c = c { memory = memory c // [(addr, val)] }

setInputs :: [Int] -> Computer -> Computer
setInputs xs c = c { inputs = xs }

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
