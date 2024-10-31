module Interpolate (
  lagrange,
  lagrangeInterpolate,
  linearInterpolate,
  processData
) where

import System.IO (hFlush, stdout)
import Parsing (Config (..), readPoint)
import Text.Printf (printf)
import Control.Monad (when)

lagrange :: Double -> [(Double, Double)] -> Double
lagrange x points = sum [y_i * product [(x - x_j) / (x_i - x_j) | (x_j, _) <- points, x_i /= x_j] | (x_i, y_i) <- points]

linearInterpolate :: Double -> (Double, Double) -> (Double, Double) -> [Double]
linearInterpolate step (x1, y1) (x2, y2) =
    let xs = takeWhile (< x2 + step) [x1, x1 + step ..]
    in map (\x -> y1 + (y2 - y1) * (x - x1) / (x2 - x1)) xs

lagrangeInterpolate :: Double -> [(Double, Double)] -> [Double]
lagrangeInterpolate step points =
    let xs = [fst (head points), fst (head points) + step .. fst (last points) + step]
    in map (\x -> lagrange x points) xs

printResult :: [Double] -> IO ()
printResult = mapM_ (putStr . printf "  %.2f")

processData :: Config -> [(Double, Double)] -> IO ()
processData config points = do
    let numPoints = length points

    when (numPoints >= 2) $ do
        let recentPoints = take 2 $ reverse points
        putStrLn $ "Линейная от " ++ show (fst (head recentPoints)) ++ " с шагом " ++ show (samplingRate config)
        let linResult = linearInterpolate (samplingRate config) (recentPoints !! 1) (recentPoints !! 0)
        printResult linResult

    when (numPoints >= 4) $ do
        let recentPoints = take 4 $ reverse points
        putStrLn $ "\nЛагранж от " ++ show (fst (last $ recentPoints)) ++ " с шагом " ++ show (samplingRate config)
        let lagResult = lagrangeInterpolate (samplingRate config) (reverse recentPoints)
        printResult lagResult

    putStrLn "\nВведите новую точку (x y):"
    hFlush stdout
    newPoint <- readPoint
    processData config (points ++ [newPoint])
