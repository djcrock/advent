import Control.Applicative ( (<|>) )
import Control.Monad.Trans.State
import Data.Functor ( ($>) )
import Text.Parsec
    ( 
      digit,
      newline,
      string,
      many1,
      oneOf,
      sepEndBy,
      space,
      parse,
      Parsec )

type Parser a = Parsec String () a
data Instruction = Acc Int | Jmp Int | Nop Int deriving (Eq, Show)
type Program = [Instruction]
type ProgramCounter = Int
type Accumulator = Int
type Visited = [ProgramCounter]

argument :: Parser Int
argument = do
    sign <- oneOf "+-"
    ds <- many1 digit
    return $ case sign of
        '+' -> read ds
        '-' -> -(read ds)

instruction :: Parser Instruction
instruction = do
    operation <- (string "acc" $> Acc)
             <|> (string "jmp" $> Jmp)
             <|> (string "nop" $> Nop)
    space
    operation <$> argument

inputParser :: Parser Program
inputParser = sepEndBy instruction newline

must (Right x) = x
must (Left y)  = error $ show y

isLoop :: State Visited Bool
isLoop = do
    vis <- get
    return (elem (head vis) (tail vis))

currentCounter :: State Visited ProgramCounter
currentCounter = head <$> get

pushProgramCounter :: ProgramCounter -> State Visited ()
pushProgramCounter p = modify (p:)

-- Run the given program until it loops or halts
runProgram :: Program -> State Visited Accumulator
runProgram prog = do
    loop <- isLoop
    counter <- currentCounter
    if loop || counter >= length prog then return 0 else do
    let instruction = prog !! counter
    case instruction of
        Acc n -> do
            pushProgramCounter (counter + 1)
            acc <- runProgram prog
            return (n + acc)
        Jmp n -> do
            pushProgramCounter (counter + n)
            runProgram prog
        Nop _ -> do
            pushProgramCounter (counter + 1)
            runProgram prog

flipInstruction :: Instruction -> Instruction
flipInstruction (Nop x) = Jmp x
flipInstruction (Jmp x) = Nop x
flipInstruction i       = i

-- Create all possible single-position nop/jmp flips
generateFixes :: Program -> [Program]
generateFixes []     = []
generateFixes (x:xs) = if flip == x
                       then fixRest
                       else (flip : xs) : fixRest
    where flip    = flipInstruction x
          fixRest = map (x:) (generateFixes xs)


partOne prog = evalState (runProgram prog) [0]
partTwo prog = (fst . head) (filter didHalt outputs)
    where outputs = runState <$> (runProgram <$> generateFixes prog) <*> pure [0]
          didHalt (_,visited) = head visited == length prog

main = interact $ (++ "\n") . show . sequence [partOne, partTwo] . must . parse inputParser ""
