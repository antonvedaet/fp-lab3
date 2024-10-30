module Main (main) where

import Parsing (parseArgs, readInput)
import Interpolate (processData)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  args <- getArgs
  let config = parseArgs args
  putStrLn "Введите x, y: "
  hFlush stdout
  points <- readInput
  processData config points
