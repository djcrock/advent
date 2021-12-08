import Text.Parsec
import Data.List ( sort )
import qualified Data.Map as Map

type Parser a = Parsec String () a
type Segment = Char
type Digit = [Segment]
type Sequence = [Digit]

segments = ['a'..'g']

inputParser :: Parser [(Sequence, Sequence)]
inputParser = sepEndBy lineP newline
    where lineP = (,) <$> (sequenceP <* string "| ") <*> sequenceP
          sequenceP = sepEndBy digitP (char ' ')
          digitP    = many1 segmentP
          segmentP  = oneOf segments

must (Right x) = x
must (Left y)  = error $ show y

digitMap :: Map.Map Digit Int
digitMap = Map.fromList
    [ ("abcefg",  0)
    , ("cf",      1)
    , ("acdeg",   2)
    , ("acdfg",   3)
    , ("bcdf",    4)
    , ("abdfg",   5)
    , ("abdefg",  6)
    , ("acf",     7)
    , ("abcdefg", 8)
    , ("abcdfg",  9) ]

segmentAttributes :: Sequence -> Segment -> (Int, Int)
segmentAttributes seq seg = (appearances, associateCount)
    where
        -- The number of digits that the segment appears in
        appearances = length $ filter (elem seg) $ seq
        -- The number of segments in digits that the segment appears in
        associateCount = sum $ map length $ filter (elem seg) $ seq

segmentAttributeMap :: Map.Map (Int, Int) Segment
segmentAttributeMap = Map.fromList $ zip attributes segments
    where sequence = Map.keys digitMap
          attributes = map (segmentAttributes sequence) segments

getSegmentMap :: Sequence -> Map.Map Segment Segment
getSegmentMap seq = foldr (\seg -> Map.insert seg (match seg)) Map.empty segments
    where attributes = map (segmentAttributes seq) segments
          match seg = segmentAttributeMap Map.! segmentAttributes seq seg

fixSegments :: (Sequence, Sequence) -> Sequence
fixSegments (seq, digits) = map translate digits
    where translate = sort . map ((getSegmentMap seq) Map.!)

digitToInt :: Digit -> Int
digitToInt = (digitMap Map.!)

sequenceToInt :: Sequence -> Int
sequenceToInt = combineDigits . map digitToInt
    where combineDigits = foldr (\i total -> (total * 10) + i) 0 . reverse

partOne = length . filter (`elem` [1,4,7,8]) . map digitToInt . concat . map fixSegments
partTwo = sum . map sequenceToInt . map fixSegments

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . must . parse inputParser ""
