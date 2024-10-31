module ParsingSpec (
  testParseArgs,
  testParseLine
  )
where

import Test.HUnit
import Parsing

testParseArgs :: Test
testParseArgs = TestCase (do
    let config = parseArgs ["linear", "lagrange", "2.5"]
    assertEqual "Algorithms should include 'linear' and 'lagrange'" ["linear", "lagrange"] (algorithms config)
    assertEqual "Sampling rate should be parsed from string" 2.5 (samplingRate config)

    let config2 = parseArgs ["badAlgo", "1.0"]
    assertEqual "No valid algorithms should be present" [] (algorithms config2)
    assertEqual "Default sampling rate should be 1.0" 1.0 (samplingRate config2))

testParseLine :: Test
testParseLine = TestCase (do
    assertEqual "Parsing valid line with numbers" (Just (1.0, 2.0)) (parseLine "1.0,2.0")
    assertEqual "Parsing invalid line" Nothing (parseLine "invalid input")
    assertEqual "Parsing line with spaces" (Just (3.0, 4.0)) (parseLine "3.0  4.0"))

