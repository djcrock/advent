import Data.Char ( digitToInt )
import Data.List ( transpose )

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

rotate :: [[a]] -> [[[a]]]
rotate = sequence [id, map reverse, transpose, map reverse . transpose]

unrotate :: [[[a]]] -> [[[a]]]
unrotate = zipWith ($) [id, map reverse, transpose, transpose . map reverse]

overlay :: (a -> a -> a) -> [[[a]]] -> [[a]]
overlay f = foldr1 (zipWith (zipWith f))

visibleOutside :: Int -> [Int] -> [Bool]
visibleOutside _ []     = []
visibleOutside n (x:xs) = (x > n) : visibleOutside (max x n) xs

visibleFromTree :: [Int] -> [Int] -> [Int]
visibleFromTree _ [] = []
visibleFromTree prev (x:xs) = numVisible : visibleFromTree (x:prev) xs
    where (seen, unseen) = span (<x) prev
          numVisible     = (length seen) + if null unseen then 0 else 1

visibilityMap :: [[Int]] -> [[Bool]]
visibilityMap = overlay (||) . unrotate . map (map (visibleOutside (-1))) . rotate

scenicScore :: [[Int]] -> [[Int]]
scenicScore = overlay (*) . unrotate . map (map (visibleFromTree [])) . rotate

partOne = length . concatMap (filter id) . visibilityMap
partTwo = maximum . concat . scenicScore

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parse
