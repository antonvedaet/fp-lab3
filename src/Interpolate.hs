module Interpolate (
  lagrange,
  lagrangeInterpolate,
  linearInterpolate,
  processData
) where

import Parsing (Config (..))
import Text.Printf (printf)
import Data.List (sortOn)

lagrange :: Double -> [(Double, Double)] -> Double
lagrange x points = sum [y_i * product [(x - x_j) / (x_i - x_j) | (x_j, _) <- points, x_i /= x_j] | (x_i, y_i) <- points]

lagrangeInterpolate :: [(Double, Double)] -> [Double] -> [(Double, Double)]
lagrangeInterpolate points xs = [(x, lagrange x points) | x <- xs]

linearInterpolate :: (Double, Double) -> (Double, Double) ->  [Double] -> [(Double, Double)]
linearInterpolate (x0, y0) (x1, y1) xs =
        let slope = (y1 - y0) / (x1 - x0)
        in [(x, y0 + slope * (x - x0)) | x <- xs, x >= x0, x<=x1]

printResult :: String -> (Double, Double) -> IO ()
printResult alg (x, y) = printf "%s: %.2f\t%.2f\n" alg x y

processData :: Config -> [(Double, Double)] -> IO ()
processData config points = do
        let sortedPoints = sortOn fst points
            xs = [fst (head sortedPoints), fst (head sortedPoints) + samplingRate config .. fst (last sortedPoints)]
            linearResults = if "linear" `elem` algorithms config
                            then concat [linearInterpolate p1 p2 xs | (p1, p2) <- zip sortedPoints(tail sortedPoints)]
                            else []
            lagrangeResults = if "lagrange" `elem` algorithms config
                              then lagrangeInterpolate sortedPoints xs
                              else []
        mapM_ (printResult "Линейная") linearResults
        mapM_ (printResult "Лагранжева") lagrangeResults
