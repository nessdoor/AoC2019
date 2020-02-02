import System.Environment
import System.IO
import qualified Data.IntMap.Strict as IMS

import IntcodeInterpreter


main = do
    (inputfile:goal:_) <- getArgs
    handle <- openFile inputfile ReadMode
    program <- hGetContents handle
    let loadNV n v = IMS.insert 1 n $ IMS.insert 2 v $ loadProgram program
        evaluateProgram n v = IMS.lookup 0 . runIntcode $ loadNV n v
        igoal = Just (read goal)
    print $ take 1 $ [100 * n + v | n <- [0..99], v <- [0..99], igoal == evaluateProgram n v]
    hClose handle
