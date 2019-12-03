module Two where

-- https://adventofcode.com/2019/day/2

type Program = [Int]

type Address = Int

data Instruction
  = Add (Int, Int, Address)
  | Multiply (Int, Int, Address)
  | Finish

-- Update list at index w/ new value - maybe there is a better way?
update :: Address -> Int -> Program -> Program
update address newValue program = a ++ (newValue : b)
  where (a, _ : b) = splitAt address program

-- Execute an instruction and return the new Program state.
execute :: Instruction -> Program -> Program
execute instruction program =
  case instruction of
    Add (a, b, address)      -> update address (a + b) program
    Multiply (a, b, address) -> update address (a * b) program
    Finish                   -> program

-- Recursivly execute instructions until Finish.
process :: Program -> Address -> Program
process program address =
  case instruction of
    Finish -> program
    _      -> process (execute instruction program) nextAddress
  where
    instruction = decode program address
    nextAddress = address + 4

-- Translate an address/opcode into an instruction.
decode :: Program -> Address -> Instruction
decode program address =
  case opCode of
    1  -> Add (p1, p2, p3)
    2  -> Multiply (p1, p2, p3)
    99 -> Finish
    _  -> error ("unknown opcode " ++ show opCode ++ " at " ++ show address)
 where
  opCode = program !! address
  p1 = program !! (program !! (address + 1)) -- Lookup the referenced value address points to.
  p2 = program !! (program !! (address + 2))
  p3 = program !! (address + 3)

-- Run the program and get the value left over at address zero - starting from address zero.
run :: Program -> Address
run program = head $ process program 0

-- Restore to pre-alarm settings.
restoreProgram :: Program -> Program
restoreProgram program = update 2 2 (update 1 12 program)

-- Setup program with some different initial values.
setNounAndVerb :: Program -> Int -> Int -> Program
setNounAndVerb program noun verb = update 2 verb (update 1 noun program)

-- Part 2, look for the noun + verb inputs that result in the magic output.
findNounAndVerb :: Program -> (Int, Int, Int)
findNounAndVerb program = head $ filter isMagicOutput $ map runWithParams searchParams
 where
  isMagicOutput (output, _, _) = output == 19690720
  runWithParams [noun, verb] = (run (setNounAndVerb program noun verb), noun, verb)
  searchParams = sequence [[0 .. 99], [0 .. 99]]

input :: Program
input =
  [ 1
  , 0
  , 0
  , 3
  , 1
  , 1
  , 2
  , 3
  , 1
  , 3
  , 4
  , 3
  , 1
  , 5
  , 0
  , 3
  , 2
  , 9
  , 1
  , 19
  , 1
  , 5
  , 19
  , 23
  , 2
  , 9
  , 23
  , 27
  , 1
  , 27
  , 5
  , 31
  , 2
  , 31
  , 13
  , 35
  , 1
  , 35
  , 9
  , 39
  , 1
  , 39
  , 10
  , 43
  , 2
  , 43
  , 9
  , 47
  , 1
  , 47
  , 5
  , 51
  , 2
  , 13
  , 51
  , 55
  , 1
  , 9
  , 55
  , 59
  , 1
  , 5
  , 59
  , 63
  , 2
  , 6
  , 63
  , 67
  , 1
  , 5
  , 67
  , 71
  , 1
  , 6
  , 71
  , 75
  , 2
  , 9
  , 75
  , 79
  , 1
  , 79
  , 13
  , 83
  , 1
  , 83
  , 13
  , 87
  , 1
  , 87
  , 5
  , 91
  , 1
  , 6
  , 91
  , 95
  , 2
  , 95
  , 13
  , 99
  , 2
  , 13
  , 99
  , 103
  , 1
  , 5
  , 103
  , 107
  , 1
  , 107
  , 10
  , 111
  , 1
  , 111
  , 13
  , 115
  , 1
  , 10
  , 115
  , 119
  , 1
  , 9
  , 119
  , 123
  , 2
  , 6
  , 123
  , 127
  , 1
  , 5
  , 127
  , 131
  , 2
  , 6
  , 131
  , 135
  , 1
  , 135
  , 2
  , 139
  , 1
  , 139
  , 9
  , 0
  , 99
  , 2
  , 14
  , 0
  , 0
  ]
