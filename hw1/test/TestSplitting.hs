module TestSplitting (testSplitting) where

import Splitting
import Test.Tasty
import Test.Tasty.HUnit
import Data.List.NonEmpty

splitOnTests :: Assertion
splitOnTests = do
    splitOn '/' "path/to/file" @?= "path" :| ["to", "file"]
    splitOn '@' "path/to@file" @?= "path/to" :| ["file"]
    splitOn '&' "path&&&to&file" @?= "path" :| ["", "", "to", "file"]

joinWithTests :: Assertion
joinWithTests = do
    joinWith '/' ("path" :| ["to", "file"]) @?= "path/to/file"
    joinWith '@' ("path/to" :| ["file"]) @?= "path/to@file"
    joinWith '&' ("path" :| ["", "", "to", "file"]) @?= "path&&&to&file"

compSplitJoinTests :: Assertion
compSplitJoinTests = do
    joinWith '/' (splitOn '/' "abacacba/sad//d/e") @?= "abacacba/sad//d/e"
    joinWith '%' (splitOn '%' "percent%percent%") @?= "percent%percent%"
    joinWith '/' (splitOn '/' "percent%percent%//") @?= "percent%percent%//"

testSplitting :: TestTree
testSplitting =
    testGroup
    "Test module Splitting"
    [ testCase "splitOn function" splitOnTests
    , testCase "joinWith function" joinWithTests
    , testCase "composition of splitOn and joinWith" compSplitJoinTests
    ]

