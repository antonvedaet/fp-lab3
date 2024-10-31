import Test.HUnit
import ParsingSpec
import InterpolateSpec

main :: IO ()
main = do
  _ <-
    runTestTT $
        TestList
            [ testParseArgs
            , testParseLine
            , testLagrange
            , testLinearInterpolation]
  return ()
