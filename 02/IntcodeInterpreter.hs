module IntcodeInterpreter (loadProgram, runIntcode) where

import qualified Data.IntMap.Strict as IMS

type Intcode = IMS.IntMap Int
type Opcode = Int
data CompState = CompState {instrPtr :: Int, memory :: Intcode, halted :: Bool}

loadProgram :: String -> Intcode
loadProgram s = IMS.fromList $ zip [0..] $ map read $ words . map replComma $ s
    where replComma ',' = ' '
          replComma c = c

retrieveOperand = IMS.findWithDefault 0

elemOp :: Opcode -> (Int, Intcode) -> (Int, Intcode)
elemOp opcode state@(ip, mem) = ((ip + 4), IMS.insert (destination state) ((case opcode of
                                                                              1 -> (+)
                                                                              2 -> (*)) (operand1 state) (operand2 state)) mem)
    where operand1 (ip, mem) = retrieveOperand (retrieveOperand (ip + 1) mem) mem
          operand2 (ip, mem) = retrieveOperand (retrieveOperand (ip + 2) mem) mem
          destination (ip, mem) = retrieveOperand (ip + 3) mem

execStep :: CompState -> CompState
execStep (CompState ip mem h) = let operation = IMS.lookup ip mem
                                 in case operation of
                                      Nothing -> error "Out of bounds"
                                      Just 99 -> CompState ip mem True
                                      Just opcode -> let (newIp, newMem) = elemOp opcode (ip, mem)
                                                      in CompState newIp newMem False

runIntcode :: Intcode -> Intcode
runIntcode program = runner $ CompState 0 program False
    where runner (CompState _ mem True) = mem
          runner state = runner $ execStep state
