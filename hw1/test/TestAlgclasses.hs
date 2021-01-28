module TestAlgclasses (testAlgclasses) where

import Algclasses
import Test.Tasty
import Test.Tasty.HUnit

--Associativity law for Semigroup
assocNonEmptyTests :: Assertion
assocNonEmptyTests = do
    let list1 = (1 :: Int) :| [3, 2]
    let list2 = 4 :| []
    let list3 = 5 :| [6]
    list1 <> (list2 <> list3) @?= (list1 <> list2) <> list3

nonEmptyTests :: Assertion
nonEmptyTests = do
   (1 :: Int) :| [] <> 2 :| [] @?= 1 :| [2]
   (0 :: Int) :| [300, 200] <> 100 :| [] @?= 0 :| [300, 200, 100]
   (11 :: Int) :| [666, 555, 1000] <> 5 :| [] @?= 11 :| [666, 555, 1000, 5]
   (1 :: Int) :| [300, 200, 500, 100] <> 5 :| [] @?= 1 :| [300, 200, 500, 100, 5]

--Associativity law for Semigroup
assocThisOfThatTests :: Assertion
assocThisOfThatTests = do
    let this1 = This "a"
    let that1 = That "b"
    let both1 = Both "a" "b"
    this1 <> (that1 <> both1) @?= (this1 <> that1) <> both1

thisOrThatTests :: Assertion
thisOrThatTests = do
   let wrapThis this = This this :: ThisOrThat String String
   let wrapThat that = That that :: ThisOrThat String String
   let wrapBoth this that = Both this that :: ThisOrThat String String
   wrapThis "aaaa" <> wrapThis "c" @?= wrapThis "aaaac"
   wrapThat "bbb" <> wrapThat "c" @?= wrapThat "bbbc"
   wrapBoth "aa" "b" <> wrapThis "c" <> wrapThat "d" @?= wrapBoth "aac" "bd"

--Associativity law for Semigroup
assocNameTests :: Assertion
assocNameTests = do
    let name1 = Name "a"
    let name2 = Name "b"
    let empty1 = Empty
    name1 <> (empty1 <> name2) @?= (name1 <> empty1) <> name2

--Identity laws for Monoid
identNameTests :: Assertion
identNameTests = do
    let name1 = Name "aa"
    name1 <> mempty @?= name1
    mempty <> name1 @?= name1

nameTests :: Assertion
nameTests = do
   let wrapName name = Name name
   let empty1 = Empty
   wrapName "aaaa" <> wrapName "c" @?= wrapName "aaaa.c"
   wrapName "bbb" <> empty1 @?= wrapName "bbb"
   wrapName "aa" <> empty1 <> wrapName "d" @?= wrapName "aa.d"

--Associativity law for Semigroup
assocEndoTests :: Assertion
assocEndoTests = do
    let func1 string = string ++ "a"
    let func2 string = "b" ++ string
    let func3 string = "c" ++ string
    let endo1 = Endo func1
    let endo2 = Endo func2
    let endo3 = Endo func3
    getEndo (endo1 <> (endo2 <> endo3)) "d" @?= getEndo ((endo1 <> endo2) <> endo3) "d"

--Identity laws for Monoid
identEndoTests :: Assertion
identEndoTests = do
    let func1 string = string ++ "aa"
    let endo1 = Endo func1
    getEndo (endo1 <> mempty) "ok" @?= "okaa"
    getEndo (mempty <> endo1) "ok" @?= "okaa"

testAlgclasses :: TestTree
testAlgclasses =
    testGroup
    "Test module Algclasses"
    [ testCase "Associativity law for Semigroup NonEmpty" assocNonEmptyTests
    , testCase "NonEmpty testing" nonEmptyTests
    , testCase "Associativity law for Semigroup ThisOrThat" assocThisOfThatTests
    , testCase "ThisOrThat testing" thisOrThatTests
    , testCase "Associativity law for Semigroup Name" assocNameTests
    , testCase "Identity law for Monoid Name" identNameTests
    , testCase "Name testing" nameTests
    , testCase "Associativity law for Semigroup Endo" assocEndoTests
    , testCase "Identity law for Monoid Endo" identEndoTests
    ]

