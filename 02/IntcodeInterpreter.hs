module IntcodeInterpreter (loadProgram, runIntcode) where

import qualified Data.IntMap.Strict as IMS

type Intcode = IMS.IntMap Int
type Opcode = Int
data CompState = CompState {instrPtr :: Int, memory :: Intcode}
type StopFlag = Bool

loadProgram :: String -> Intcode
loadProgram s = IMS.fromList $ zip [0..] $ map read $ words . map replComma $ s
    where replComma ',' = ' '
          replComma c = c

retrieveOperand = IMS.findWithDefault 0

elemOp :: Opcode -> CompState -> CompState
elemOp opcode state@(CompState ip mem) = CompState (ip + 4) $ IMS.insert (destination state) ((case opcode of
                                                                                                 1 -> (+)
                                                                                                 2 -> (*)) (operand1 state) (operand2 state)) mem
    where operand1 (CompState ip mem) = retrieveOperand (retrieveOperand (ip + 1) mem) mem
          operand2 (CompState ip mem) = retrieveOperand (retrieveOperand (ip + 2) mem) mem
          destination (CompState ip mem) = retrieveOperand (ip + 3) mem

execStep :: CompState -> (CompState, StopFlag)
execStep state@(CompState ip mem) = let operation = IMS.lookup ip mem
                                     in case operation of
                                          Nothing -> error "Out of bounds"
                                          Just 99 -> (state, True)
                                          Just opcode -> (elemOp opcode state, False)

runIntcode :: Intcode -> Intcode
runIntcode program = runner (CompState 0 program, False)
    where runner (st, True) = memory st
          runner (st, False) = runner $ execStep st
