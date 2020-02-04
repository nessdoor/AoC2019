import System.Environment
import System.IO
import qualified Data.List as DL

import WireAnalyzer

origin = (0, 0)

main = do
    (inputName:_) <- getArgs
    handle <- openFile inputName ReadMode
    cable1 <- hGetLine handle
    cable2 <- hGetLine handle
    let crossingPoints = [intersectionPoints c1 c2 |
                          c1 <- loadCable . loadPathElems $ cable1,
                          c2 <- loadCable . loadPathElems $ cable2,
                          intersectsWith c1 c2]
        -- Find the intersection point nearest to the origin, excluding the origin itself
        nearestPoint = DL.minimumBy (\p1 p2 -> compare (mHDistance origin p1) (mHDistance origin p2)) . filter (/= (0, 0)). concat $ crossingPoints
    print $ mHDistance origin nearestPoint
    hClose handle
