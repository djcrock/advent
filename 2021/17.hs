import Control.Monad ( liftM2 )
import Data.List ( inits )
import Text.Parsec ( (<|>), Parsec, char, digit, many1, parse, string, try )

type Parser a = Parsec String () a
type Vec2 = (Int,Int)
type Rectangle = (Vec2,Vec2)

inputP :: Parser Rectangle
inputP = do
    string "target area: x="
    (x1,x2) <- rangeP
    string ", y="
    (y1,y2) <- rangeP
    pure ((min x1 x2, min y1 y2), (max x1 x2, max y1 y2))
    where rangeP = (,) <$> (numP <* string "..") <*> numP
          numP   = try negP <|> natP
          negP   = negate <$> (char '-' *> natP)
          natP   = read <$> many1 digit

must (Right x) = x
must (Left y)  = error $ show y

between :: Int -> Int -> Int -> Bool
between start end n = start <= n && n <= end

xVelocities :: Rectangle -> [Int]
xVelocities ((x1,_),(x2,_)) = filter validVelocity [minV..maxV]
          -- The first vX that actually gets the probe to the target zone
    where minV = length . head . dropWhile ((<x1) . sum) $ inits [1..]
          -- A one-tick "laser beam" shot
          maxV = x2
          validVelocity vX = any (between x1 x2 . sum) $ inits [vX,vX-1..0]

-- When shot upward with velocity vY, the probe returns to y=0 with velocity -vY
-- The next tick will bring it to y=-(vY+1)
-- Thus, the maximum vY to avoid passing the bottom of the target is abs(y1)-1
yVelocities :: Rectangle -> [Int]
yVelocities ((_,y1),(_,y2)) = filter validVelocity [minV..maxV]
          -- A one-tick "laser beam" shot
    where minV = y1
          maxV = (abs y1)-1
          validVelocity vY = any (between y1 y2) $ takeWhile (>=y1) $ map sum $ inits [vY,vY-1..]

apex :: Int -> Int
apex vY = sum [0..vY]

validVelocities :: Rectangle -> [Vec2]
validVelocities t = filter isValid possibleVelocities
    where possibleVelocities = (liftM2 (,)) (xVelocities t) (yVelocities t)
          isValid v = any (isInRect t) $ takeWhile (not . pastTarget t) $ path v
          path v = iterate tick ((0,0),v)
          tick ((pX,pY),(vX,vY)) = ((pX+vX,pY+vY), (vX-(signum vX),vY-1))
          pastTarget ((_,y1),(x2,_)) ((x,y),_) = x > x2 || y < y1
          isInRect ((x1,y1),(x2,y2)) ((x,y),_) = (between x1 x2 x) && (between y1 y2 y)

-- As long as you pick a vX that decays to 0 (such as the minimum vX),
-- you can pick any valid vY. Thus, we can ignore vX for partOne.
partOne = maximum . map apex . yVelocities
partTwo = length . validVelocities

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . must . parse inputP ""
