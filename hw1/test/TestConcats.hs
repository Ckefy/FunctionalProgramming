module TestConcats (testConcats) where

import Concats
import Test.Tasty
import Test.Tasty.HUnit

maybeConcatTests :: Assertion
maybeConcatTests = do
    maybeConcat [Just [1, 2, 3], Nothing, Just [4, 5]] @?= [1, 2, 3, 4, 5]
    maybeConcat [Just [1], Just[2], Just[3]] @?= [1, 2, 3]
    maybeConcat [Nothing :: Maybe[Int]] @?= []
    maybeConcat [Nothing :: Maybe[Int], Nothing, Nothing] @?= []
    maybeConcat [Just [] :: Maybe[Int]] @?= []

eitherConcatTests :: Assertion
eitherConcatTests = do
    eitherConcat [Left "abc", Right [3, 2, 1], Right [4, 5], Right [6], Left "dr"] @?= ("abcdr", [3, 2, 1, 4, 5, 6])
    eitherConcat [Left "abc" :: Either [Char][Int]] @?= ("abc", [])
    eitherConcat [Right [4, 5], Right [6]] @?= ("", [4, 5, 6])
    eitherConcat [Left "abc", Right [1, 2]] @?= ("abc", [1, 2])

testConcats :: TestTree
testConcats =
    testGroup
    "Test module Concats"
    [ testCase "maybeConcat function" maybeConcatTests
    , testCase "eitherConcat function" eitherConcatTests
    ]

