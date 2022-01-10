import Advent
import Data.Array
import Data.Char ( chr, ord )
import Data.List ( elemIndex, inits, intersperse, sortOn )
import Data.Maybe ( fromJust )
import Data.Tuple ( swap )

data Move = R Int | L Int deriving Eq
type Function = [Move]
type Image = String
type ImageArray = Array Vec2 Char

instance Show Move where
    show (R x) = "R," ++ show x
    show (L x) = "L," ++ show x

readASCII :: Computer -> (String,Computer)
readASCII = swap . fmap (map chr) . swap . allOutput . run

imageArray :: Image -> ImageArray
imageArray img = listArray ((0,0),(maxRow,maxCol)) (concat $ lines img)
    where maxRow = (length $ lines img) - 2
          maxCol = (length $ head $ lines img) - 1

runPrompt :: [String] -> Computer -> ([String],Computer)
runPrompt [] c         = swap $ fmap (:[]) $ swap $ readASCII c
runPrompt (cmd:cmds) c = (out : (cmd ++ "\n") : rest, c'')
    where (out,c')   = readASCII c
          (rest,c'') = runPrompt cmds (setInputs (map ord (cmd ++ "\n")) c')

getImg :: ImageArray -> Vec2 -> Char
getImg img coord= if (inRange (bounds img) coord) then img ! coord else '.'

alignmentParameter :: ImageArray -> Vec2 -> Int
alignmentParameter img coord@(r,c) = if all (== '#') tiles then r*c else 0
    where coords = coord : orthoAdjacent coord
          tiles  = map (getImg img) coords

navigate :: ImageArray -> [Move]
navigate img = if finished then [] else (move : navigate newImg)
    where ((pr,pc),(dr,dc)) = fmap heading $ head
                                $ filter (flip elem "^>v<" . snd) $ assocs img
          left  = getImg img (vec2Add (pr,pc) (-dc, dr))
          right = getImg img (vec2Add (pr,pc) ( dc,-dr))
          countScaf dir = length $ takeWhile ((== '#') . getImg img)
                            $ tail $ iterate (vec2Add dir) (pr,pc)
          numLeft  = countScaf (-dc, dr)
          numRight = countScaf ( dc,-dr)
          finished = numLeft + numRight == 0
          move     = if numLeft > numRight then L numLeft else R numRight
          facing   = if numLeft > numRight then (-dc,dr)  else (dc,-dr)
          newTile  = tile facing
          newPos   = (iterate (vec2Add facing) (pr,pc)) !! max numLeft numRight
          newImg   = img // [((pr,pc),'#'),(newPos,newTile)]
          tile d   = case d of 
                        (-1, 0) -> '^'
                        ( 0, 1) -> '>'
                        ( 1, 0) -> 'v'
                        ( 0,-1) -> '<'
          heading c = case c of
                        '^' -> (-1, 0)
                        '>' -> ( 0, 1)
                        'v' -> ( 1, 0)
                        '<' -> ( 0,-1)

decompose :: Int -> [[Move]] -> [[Function]]
decompose _ [] = [[]]
decompose 0 _  = []
decompose n xs = sortOn length $ do
    func <- tail $ inits $ head xs
    let rest = concatMap (deleteAll [] . splitOnList func) xs
    others <- decompose (n-1) rest
    pure (func : others)

substituteFuncs :: [Move] -> [Function] -> [String]
substituteFuncs moves funcs = (intersperse ',' calls)
                              : map (concat . intersperse "," . map show) funcs
    where chunked = fromJust $ chunksOf funcs moves
          calls   = map (("ABC" !!) . fromJust . flip elemIndex funcs) chunked

movesToProgram :: [Move] -> [String]
movesToProgram moves = minimumOn (length . concat) $ map (substituteFuncs moves) fss
    where fss = decompose 3 [moves]

partOne c = show $ sum $ map (alignmentParameter img) (indices img)
    where img = imageArray $ fst $ readASCII c
partTwo c = unlines $ intersperse "" $ [concat out, show res]
    where (img,_)  = readASCII c
          nav      = navigate $ imageArray img
          prog     = (movesToProgram nav) ++ ["n"]
          (out,c') = runPrompt prog (poke 0 2 c)
          res      = ord $ last $ last out
          --res      = fst $ mustPopOutput $ run $ setInputs (map ord prog) (poke 0 2 c)
          steps    = map fst $ take 1 $ iterate (readASCII . snd) (img,c')
          

main = runSolutionsStr [partOne, partTwo] readComputer
