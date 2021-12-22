import Text.Parsec

type Range = (Int,Int)
type Cuboid = [Range]
type Instruction = (Bool,Cuboid)

parseInput :: String -> [Instruction]
parseInput = must . parse (sepEndBy instP newline) ""
    where instP   = (,) <$> (powerP <* space) <*> (sepBy rangeP (char ','))
          powerP  = try (string "on"  *> pure True)
                    <|> (string "off" *> pure False)
          rangeP  = (,) <$> (oneOf "xyz" *> char '=' *> numP)
                        <*> (string ".." *> numP)
          numP    = try negP <|> natP
          negP    = negate <$> (char '-' *> natP)
          natP    = read <$> many1 digit
          must    = either (error . show) id

touches :: Cuboid -> Cuboid -> Bool
touches [] _ = True
touches ((m1,m2):ms) ((t1,t2):ts) = t1 <= m2 && m1 <= t2 && touches ms ts

-- Apply a mask to a cuboid, breaking it into up to 6 pieces
split :: Cuboid -> Cuboid -> [Cuboid]
split _ [] = []
split ((m1,m2):masks) ((t1,t2):targets) =
    (map (:targets) splits) ++ map (limit:) (split masks targets)
    where (splits,limit)
            | m1 <= t1 && m2 >= t2 = ([],                     (t1,t2))
            | m1 > t1  && m2 < t2  = ([(t1,m1-1), (m2+1,t2)], (m1,m2))
            | m1 <= t1             = ([(m2+1,t2)],            (t1,m2))
            | m2 >= t2             = ([(t1,m1-1)],            (m1,t2))

on :: Cuboid -> [Cuboid] -> [Cuboid]
on mask ons = ons ++ (foldr off [mask] ons)

off :: Cuboid -> [Cuboid] -> [Cuboid]
off mask = concatMap (\c -> if touches mask c then split mask c else [c])

apply :: [Cuboid] -> Instruction -> [Cuboid]
apply ons (action,mask) = (if action then on else off) mask ons

isInitializer :: Instruction -> Bool
isInitializer = all ((<=50) . abs) . concatMap (sequence [fst,snd]) . snd

volume :: Cuboid -> Int
volume = product . map ((+1) . abs . uncurry (-))

partOne = sum . map volume . foldl apply [] . filter isInitializer
partTwo = sum . map volume . foldl apply []

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . parseInput
