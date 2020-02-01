import System.IO
import System.Environment

main = do
    (inputfile:rest) <- getArgs
    withFile inputfile ReadMode (\handle -> do
        masses <- hGetContents handle
        print . calculateFuel . map read $ lines masses)

calculateFuel :: (Integral n) => [n] -> n
calculateFuel = foldl (\acc m -> fuelClosure m + acc) 0
    where fuelClosure m = let fuel = div m 3 - 2
                          in if fuel <= 0 then
                                          0
                                          else
                                          fuel + fuelClosure fuel
