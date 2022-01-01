module Advent where

import Control.Applicative
import Control.Monad
import Control.Monad.State hiding ( fix )
import Data.Array

runSolutions :: Show b => [(a -> b)] -> (String -> a) -> IO ()
runSolutions ss parser = interact $ (++ "\n") . show . sequence ss . parser

-- Parsers

type Parser a = (String -> a)

readLines :: Read a => Parser [a]
readLines = map read . lines

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn c (x:xs)
    | x == c    = "" : segment : segments
    | otherwise = (x:segment) : segments
    where (segment:segments) = splitOn c xs

readCommaSep :: Read a => Parser [a]
readCommaSep = map read . splitOn ','

-- Intcode

type Address = Int
type Memory = Array Address Int
type Mutation = State Computer ()
type Stateful a = State Computer a
data Computer = Computer
    { instPtr :: Address
    , memory  :: Memory
    } deriving (Eq, Show)

readMemory :: Parser Memory
readMemory str = listArray (0, length ints - 1) ints
    where ints = readCommaSep str

readComputer :: Parser Computer
readComputer str = let intCode = readMemory str in Computer
    { instPtr = 0
    , memory  = intCode }

getPtr :: State Computer Address
getPtr = gets instPtr

incPtr :: Int -> Mutation
incPtr n = modify $ \c -> c { instPtr = instPtr c + n }

getAddr :: Address -> Stateful Int
getAddr addr = gets $ ((! addr) . memory)

deref :: Address -> Stateful Int
deref = getAddr <=< getAddr

setAddr :: Address -> Int -> Mutation
setAddr addr val = modify $ \c -> c { memory = memory c // [(addr,val)] }

offset :: (Address -> Stateful a) -> Int -> Stateful a
offset f n = gets instPtr >>= f . (+n)

param :: Int -> Stateful Int
param = offset getAddr

paramDeref :: Int -> Stateful Int
paramDeref = offset deref

outputParam :: Int -> Int -> Mutation
outputParam p val = param p >>= flip setAddr val

binaryOp :: (Int -> Int -> Int) -> Mutation
binaryOp f = do
    x <- paramDeref 1
    y <- paramDeref 2
    outputParam 3 (f x y)
    incPtr 4

opAdd  = binaryOp (+)
opMul  = binaryOp (*)
opHalt = pure ()

step :: Mutation
step = do
    opcode <- param 0
    case opcode of
        1  -> opAdd
        2  -> opMul
        99 -> opHalt

run :: Computer -> Computer
run = fix (execState step)

peek :: Address -> Computer -> Int
peek n = (! n) . memory

poke :: Address -> Int -> Computer -> Computer
poke addr val c = c { memory = memory c // [(addr, val)] }

-- Helpful functions

-- Iterate a function until the result stops changing, returning the final value
fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == f x then x else fix f (f x)

-- Iterate a function until the result stops changing, returning each value
iterateFix :: Eq a => (a -> a) -> a -> [a]
iterateFix f x = if x == f x then [x] else x : iterateFix f (f x)

-- Vectors

type Vec2 = (Int,Int)

vec2Op :: (Int -> Int -> Int) -> Vec2 -> Vec2 -> Vec2
vec2Op f (x1,y1) (x2,y2) = (f x1 x2, f y1 y2)

vec2Add :: Vec2 -> Vec2 -> Vec2
vec2Add = vec2Op (+)

manhattan :: Vec2 -> Vec2 -> Int
manhattan (x1,y1) (x2,y2) = (abs (x2-x1)) + (abs (y2-y1))
