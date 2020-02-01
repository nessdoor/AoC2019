import System.IO
import System.Environment

main = do
    (inputfile:rest) <- getArgs
    ih <- openFile inputfile ReadMode
    masses <- hGetContents ih
    print . calculateFuel . map read . lines $ masses
    hClose ih

calculateFuel :: (Integral n) => [n] -> n
calculateFuel = foldl (\acc m -> div m 3 -2 + acc) 0
