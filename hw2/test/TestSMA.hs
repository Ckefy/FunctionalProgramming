module TestSMA (testSMA) where

import SMA
import Test.Tasty
import Test.Tasty.HUnit

justCalculateTests :: Assertion
justCalculateTests = do
    moving 6 [1 :: Int, 5, 3, 8, 7, 9, 6] @?= [1.0 :: Double, 3.0, 3.0, 4.25, 4.8, 5.5, 6.333333333333333]
    moving 5 [1 :: Int, 5, 3, 8, 7, 9, 6] @?= [1.0 :: Double, 3.0, 3.0, 4.25, 4.8, 6.4, 6.6]
    moving 4 [1 :: Int, 5, 3, 8, 7, 9, 6] @?= [1.0 :: Double, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]
    moving 3 [1 :: Int, 5, 3, 8, 7, 9, 6] @?= [1.0 :: Double, 3.0, 3.0, 5.333333333333333, 6.0, 8.0, 7.333333333333333]
    moving 2 [1 :: Int, 5, 3, 8, 7, 9, 6] @?= [1.0 :: Double, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]
    moving 1 [1 :: Int, 5, 3, 8, 7, 9, 6] @?= [1.0 :: Double, 5.0, 3.0, 8.0, 7.0, 9.0, 6.0]

testSMA :: TestTree
testSMA =
    testGroup
    "Test module Arithmetic"
    [ testCase "Just few testing examples for SMA" justCalculateTests
    ]
