import Data.Array

type Coord = (Int,Int)
type Grid = Array Coord Char

parse :: String -> Grid
parse input = listArray ((0,0),(maxRow,maxCol)) (concat $ lines input)
    where maxRow = (length $ lines input) - 1
          maxCol = (length $ head $ lines input) - 1

north g (r,c) = g ! ((,c) $ if r-1 < 0 then (fst $ snd $ bounds g) else r-1)
west  g (r,c) = g ! ((r,) $ if c-1 < 0 then (snd $ snd $ bounds g) else c-1)
south g (r,c) = g ! ((,c) $ if r+1 > (fst $ snd $ bounds g) then 0 else r+1)
east  g (r,c) = g ! ((r,) $ if c+1 > (snd $ snd $ bounds g) then 0 else c+1)

tickEast :: Grid -> Grid
tickEast g = g // (map tickCell (assocs g))
    where tickCell (c,'.') = (c,) $ if (west g c) == '>' then '>' else '.'
          tickCell (c,'>') = (c,) $ if (east g c) == '.' then '.' else '>'
          tickCell (c,'v') = (c,'v')

tickSouth :: Grid -> Grid
tickSouth g = g // (map tickCell (assocs g))
    where tickCell (c,'.') = (c,) $ if (north g c) == 'v' then 'v' else '.'
          tickCell (c,'v') = (c,) $ if (south g c) == '.' then '.' else 'v'
          tickCell (c,'>') = (c,'>')

tick :: Grid -> Grid
tick = tickSouth . tickEast

iterateFix :: Eq a => (a -> a) -> a -> [a]
iterateFix f x = if x == f x then [x] else x : iterateFix f (f x)

main = interact $ (++ "\n") . show . length . iterateFix tick . parse
