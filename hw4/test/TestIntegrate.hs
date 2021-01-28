module TestIntegrate where

import Test.Tasty
import Test.Tasty.HUnit
import Integrate
import Text.Printf

integrateParallelTests :: Assertion
integrateParallelTests = do
      let eps = 0.1
      let f1 = cos
      let n = 1200000 :: Int
      test1 <- integrate f1 2 4 n
      let test1Real = -1.6661
      let finalTest1 = abs (test1 - test1Real) <= eps
      finalTest1 @?= True

      let f2 = sin
      test2 <- integrate f2 (-10) 10 n
      let test2Real = 0
      let finalTest2 = abs (test2 - test2Real) <= eps
      finalTest2 @?= True

      let f3 x = x * x * x + sin x + 1 / (x * x)
      test3 <- integrate f3 5 10 n
      let test3Real = 2345.0
      let finalTest3 = abs (test3 / test3Real - 1) <= eps
      finalTest3 @?= True

testIntegrate :: TestTree
testIntegrate =
    testGroup
    "Test module Integrate (2nd task)"
    [testCase "Parallel integral" integrateParallelTests
   ]