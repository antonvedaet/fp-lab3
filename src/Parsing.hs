{-# LANGUAGE GADTs #-}
module Parsing
    ( parseArgs,
      parseLine,
      readPoint,
      Config(..)
    ) where

import Data.Maybe (mapMaybe, Maybe)
import Text.Read (readMaybe)

data Config where
  Config :: {algorithms :: [String], samplingRate :: Double} ->
              Config

parseArgs :: [String] -> Config
parseArgs args = Config {
  algorithms = filter (`elem` ["linear", "lagrange"]) args,
  samplingRate = case mapMaybe readMaybe args of
                   (x:_) -> x
                   [] -> 1.0
}

parseLine :: String -> Maybe (Double, Double)
parseLine line = case words (map replaceComma line) of
                   [xString, yString] -> do
                     x <- readMaybe xString
                     y <- readMaybe yString
                     return (x, y)
                   _ -> Nothing
                where
                  replaceComma ',' = ' '
                  replaceComma c = c

readPoint :: IO (Double, Double)
readPoint = do
    line <- getLine
    case parseLine line of
        Just p  -> return p
        Nothing -> putStrLn "Неверный ввод, попробуйте снова:" >> readPoint
