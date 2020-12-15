import qualified Data.IntMap as Map

parse :: String -> [Int]
parse = map read . lines

-- Given a target position n and a stack of previous numbers,
-- determine what the nth number spoken will be.
countTo :: Int -> [Int] -> Int
countTo target prev = countToInternal
                        target
                        (Map.fromList (zip (init prev) [1..]))
                        (last prev)
                        (length prev)

-- Uses a Map to keep track of just the most recent occurrence of each number.
-- Compile with optimizations to avoid getting OOM killed. Thanks, Haskell :P
countToInternal :: Int -> Map.IntMap Int -> Int -> Int -> Int
countToInternal target recent prevVal prevIndex =
  if prevIndex == target
  then prevVal
  else countToInternal target newRecent nextVal (prevIndex + 1)
    where newRecent = Map.insert prevVal prevIndex recent
          nextVal   = case Map.lookup prevVal recent of
                        Nothing -> 0
                        Just y  -> prevIndex - y

partOne = countTo 2020
partTwo = countTo 30000000

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
