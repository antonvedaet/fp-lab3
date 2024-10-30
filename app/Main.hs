module Main (main) where

import Parsing (parseArgs, readPoint)
import Interpolate (processData)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    args <- getArgs
    let config = parseArgs args
    putStrLn "Введите первую точку (x y):"
    hFlush stdout
    firstPoint <- readPoint
    putStrLn "Введите вторую точку (x y):"
    hFlush stdout
    secondPoint <- readPoint
    processData config [firstPoint, secondPoint]
