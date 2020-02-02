import System.Environment
import System.IO
import qualified Data.IntMap.Strict as IMS

import IntcodeInterpreter


main = do
    (inputfile:_) <- getArgs
    handle <- openFile inputfile ReadMode
    program <- hGetContents handle
    print $ IMS.lookup 0 $ runIntcode . loadProgram $ program
    hClose handle
