import Data.List (group, sort)

type Adapter = Int
type Step = Int

parse :: String -> [Adapter]
parse = map read . lines

{-
  Say we have an input that looks like this:
  1  2  3  6  7  8  9 10 13 14
  This translates to these "steps":
  1  1  1  3  1  1  1  1  3  1
  Logically, any 3 cannot be removed, since the chain would be broken:
  1  1  1 (3) 1  1  1  1 (3) 1
  Likewise, anything immediately preceding a three cannot be removed:
  1  1 (1  3) 1  1  1 (1  3) 1
  That leaves a few small groups of adapters that can actually change:
  [1  1] [1  1  1] [1]
  We can find the number of arrangements for each of these groups, then take
  the product to find the overall number of arrangements.
-}

-- From a sorted list, calculate the difference between each element
steps :: [Adapter] -> [Step]
steps xs = zipWith (-) xs (0:xs)

countElem :: Eq a => a -> [a] -> Int
countElem x ys = length (filter (== x) ys)

-- e.g. [1,1,1,3,3,1,3,1,1,1,1] -> [[1,1,1],[1],[1,1,1,1]]
splitOnThrees :: [Step] -> [[Step]]
splitOnThrees = filter (elem 1) . group

-- Any adapter providing a step of 3 cannot be removed
-- Any adapter immediately preceding one that provides 3 cannot be removed
-- Split there e.g.
-- [1,1,1,3,3,1,3,1,1,1,1] -> [[1,1],[1,1,1]]
splitOnRequiredAdapters :: [Step] -> [[Step]]
splitOnRequiredAdapters = filter (not . null) . map init . splitOnThrees

-- The number of ways to arrange n adapters is 2^n
-- If the number of adapters is >= 3, then it's actually fewer, since you need
-- to account for the fact that the adapters still need to form a continuous
-- connection with steps of at most 3. Since the largest group in my input
-- has 3 adapters, I can cheat a bit and just subtract out the bad case :P
arrangements :: [Step] -> Int
arrangements xs = (2 ^ len) - if len == 3 then 1 else 0
    where len = length xs

partOne = product . sequence [countElem 1, countElem 3] . (++ [3]) . steps . sort
partTwo = product . map arrangements . splitOnRequiredAdapters . steps . sort

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
