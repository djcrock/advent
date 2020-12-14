import Data.Bits
import qualified Data.IntMap as Map
import Text.Parsec

type Address = Int
type MaskTemplate = String
type Mask = (Int,Int)
data Instruction = Store Address Int | SetMask MaskTemplate deriving (Eq, Show)
type Program = [Instruction]
type Memory = Map.IntMap Int

inputParser :: Parsec String () Program
inputParser = sepEndBy (try store <|> setMask) newline
    where nat     = read    <$> many1 digit
          store   = Store   <$> (string "mem[" *> nat <* string "] = ") <*> nat
          setMask = SetMask <$> (string "mask = " *> many1 (oneOf "X01"))

must (Right x) = x
must (Left y)  = error $ show y

binaryToInt :: [Bool] -> Int
binaryToInt = foldl (\int bit -> int * 2 + fromEnum bit) 0

toMask :: MaskTemplate -> Mask
toMask template = (orMask,andMask)
    where orMask  = binaryToInt (map (== '1') template)
          andMask = binaryToInt (map (/= '0') template)

applyMask :: Mask -> Int -> Int
applyMask (orMask,andMask) val = (val .|. orMask) .&. andMask

runV1 :: Program -> Mask -> Memory -> Memory
runV1 []                        _ mem = mem
runV1 ((SetMask newMask):insts) _ mem = runV1 insts (toMask newMask) mem
runV1 ((Store addr val):insts) mask mem =
    let newVal = applyMask mask val in
    runV1 insts mask (Map.insert addr newVal mem)

-- Translate the floating "X" bits in a mask into concrete 0/1 values.
-- Existing "0" bits are translated into "X" bits.
-- e.g. "0X1" -> ["X01","X11"]
expandFloatBits :: MaskTemplate -> [MaskTemplate]
expandFloatBits ""         = [""]
expandFloatBits ('0':bits) = map ('X':) (expandFloatBits bits)
expandFloatBits ('1':bits) = map ('1':) (expandFloatBits bits)
expandFloatBits ('X':bits) = map ('0':) (expandFloatBits bits)
                          ++ map ('1':) (expandFloatBits bits)

genMemMasks :: MaskTemplate -> [Mask]
genMemMasks = map toMask . expandFloatBits

multiInsert :: [Int] -> a -> Map.IntMap a -> Map.IntMap a
multiInsert []         _   m = m
multiInsert (key:keys) val m = multiInsert keys val (Map.insert key val m)

runV2 :: Program -> [Mask] -> Memory -> Memory
runV2 []                        _ mem = mem
runV2 ((SetMask newMask):insts) _ mem = runV2 insts (genMemMasks newMask) mem
runV2 ((Store addr val):insts) masks mem =
    let addrs = (`applyMask` addr) <$> masks in
    runV2 insts masks (multiInsert addrs val mem)

sumMemory :: Memory -> Int
sumMemory = Map.foldr (+) 0

partOne prog = sumMemory (runV1 prog (0,0) Map.empty)
partTwo prog = sumMemory (runV2 prog []    Map.empty)

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . must . parse inputParser ""
