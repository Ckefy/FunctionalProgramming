module TestStringSum (testStringSum) where

import StringSum
import Test.Tasty
import Test.Tasty.HUnit

sumTests :: Assertion
sumTests = do
    stringSum "1 2 3 4" @?= Just 10
    stringSum "1" @?= Just 1
    stringSum "-5 10" @?= Just 5
    stringSum "-5     10" @?= Just 5
    stringSum "1 2 3 4 f" @?= Nothing
    stringSum "" @?= Just 0
    stringSum "a b c" @?= Nothing
    stringSum "x + y = z" @?= Nothing
    stringSum "100 200 300 400 500" @?= Just 1500


testStringSum :: TestTree
testStringSum =
    testGroup
    "Test module StringSum"
    [ testCase "Just tests complete work of stringSum" sumTests
    ]
