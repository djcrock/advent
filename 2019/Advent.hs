module Advent where

type Parser a = (String -> a)

readLines :: Read a => Parser [a]
readLines = map read . lines

runSolutions :: Show b => [(a -> b)] -> (String -> a) -> IO ()
runSolutions ss parser = interact $ (++ "\n") . show . sequence ss . parser

-- Iterate a function until the result stops changing, returning the final value
fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == f x then x else fix f (f x)

-- Iterate a function until the result stops changing, returning each value
iterateFix :: Eq a => (a -> a) -> a -> [a]
iterateFix f x = if x == f x then [x] else x : iterateFix f (f x)
