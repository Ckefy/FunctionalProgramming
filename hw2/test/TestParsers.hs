module TestParsers (testParsers) where

import Parsers
import Test.Tasty
import Test.Tasty.HUnit

import Data.Maybe (fromJust, isNothing)
import Data.Char (isDigit)

takeFirst :: (a -> b) -> (a, c) -> (b, c)
takeFirst f (a, c) = (f a, c)

showParsed :: Show a => Maybe (a, String) -> String
showParsed parsed = fst(fromJust(fmap (takeFirst show) parsed))

cbsTests :: Assertion
cbsTests = do
    let input = runParser cbsParser ""
    let input2 = runParser cbsParser "()()"
    let input3 = runParser cbsParser "()()()()()"
    let input4 = runParser cbsParser "(())"
    let input5 = runParser cbsParser "(((())))()"
    showParsed input @?= ""
    showParsed input2 @?= "()()"
    showParsed input3 @?= "()()()()()"
    showParsed input4 @?= "(())"
    showParsed input5 @?= "(((())))()"

nonCbsTests :: Assertion
nonCbsTests = do
    let input = runParser cbsParser "(("
    let input2 = runParser cbsParser "))"
    let input3 = runParser cbsParser "(())()()()())"
    let input4 = runParser cbsParser "(()))"
    let input5 = runParser cbsParser "(((())))())()"
    isNothing input @?= True
    isNothing input2 @?= True
    isNothing input3 @?= True
    isNothing input4 @?= True
    isNothing input5 @?= True

numberParserTests :: Assertion
numberParserTests = do
    let input = runParser numbParser "123"
    let input2 = runParser numbParser "+123"
    let input3 = runParser numbParser "-123"
    let input4 = runParser numbParser "555"
    let input5 = runParser numbParser "6767676"
    showParsed input @?= "123"
    showParsed input2 @?= "123"
    showParsed input3 @?= "-123"
    showParsed input4 @?= "555"
    showParsed input5 @?= "6767676"

satisfyTests :: Assertion
satisfyTests = do
    let input = runParser (satisfy isDigit) "111"
    let input2 = runParser (satisfy isDigit) "1kek"
    let input3 = runParser (satisfy isDigit) "lol"
    let input4 = runParser (satisfy isDigit) "123456"
    let input5 = runParser (satisfy isDigit) "4"
    input @?= Just('1', "11")
    input2 @?= Just('1', "kek")
    input3 @?= Nothing
    input4 @?= Just('1', "23456")
    input5 @?= Just('4', "")

elementTests :: Assertion
elementTests = do
    let input = runParser (element '1') "111"
    let input2 = runParser (element '2') "1kek"
    let input3 = runParser (element 'l') "lol"
    let input4 = runParser (element 'a') "a23456"
    let input5 = runParser (element '4') "4"
    input @?= Just('1', "11")
    input2 @?= Nothing
    input3 @?= Just ('l', "ol")
    input4 @?= Just('a', "23456")
    input5 @?= Just('4', "")

streamTests :: Assertion
streamTests = do
    let input = runParser (stream "11") "111"
    let input2 = runParser (stream "2") "1kek"
    let input3 = runParser (stream "lo") "lol"
    let input4 = runParser (stream "aaaaaaa") "aaaaaaaa23456"
    let input5 = runParser (stream "4") "4"
    input @?= Just("11", "1")
    input2 @?= Nothing
    input3 @?= Just ("lo", "l")
    input4 @?= Just("aaaaaaa", "a23456")
    input5 @?= Just("4", "")

okTests :: Assertion
okTests = do
    let input = runParser ok "111"
    let input2 = runParser ok "1kek"
    let input3 = runParser ok [2, 3, 5]
    let input4 = runParser ok "aaaaaaaa23456"
    let input5 = runParser ok "4"
    input @?= Just((), "111")
    input2 @?= Just((), "1kek")
    input3 @?= Just((), [2, 3, 5])
    input4 @?= Just((), "aaaaaaaa23456")
    input5 @?= Just((), "4")

eofTests :: Assertion
eofTests = do
    let input = runParser eof "111"
    let input2 = runParser eof ""
    let input3 = runParser eof "lol"
    let input4 = runParser eof ""
    let input5 = runParser eof "4"
    input @?= Nothing
    input2 @?= Just((), "")
    input3 @?= Nothing
    input4 @?= Just((), "")
    input5 @?= Nothing

listParserTests :: Assertion
listParserTests = do
    let list = runParser listParser "2, 1, +10"
    let list2 = runParser listParser "3, 1, -110, +110"
    let list3 = runParser listParser "1, 0"
    let list4 = runParser listParser ""
    let list5 = runParser listParser "6, 6, 66, 666, 6666, 66666, 666666"
    list @?= Just ([1 :: Int, 10], "")
    list2 @?= Just ([1 :: Int, -110, 110], "")
    list3 @?= Just ([0 :: Int], "")
    list4 @?= Nothing
    list5 @?= Just ([6 :: Int, 66, 666, 6666, 66666, 666666], "")

listlistParserTests :: Assertion
listlistParserTests = do
    let list = runParser listlistParser "2, 1,+10  , 3,5,-7, 2"
    let list2 = runParser listlistParser "3, 1,-10 , 1 , 3,5,-7, 2"
    let list3 = runParser listlistParser "1, 1  , 3,5,-7, 2"
    let list4 = runParser listlistParser "2, 1,+10, a  , 3,5,-7, 2"
    let list5 = runParser listlistParser "3, 6, 66, 666  , 2,6666, 66666"
    list @?= Just ([ [1, 10], [5, -7, 2] ], "")
    list2 @?= Just ([ [1, -10, 1], [5, -7, 2] ], "")
    list3 @?= Just ([ [1], [5, -7, 2] ], "")
    list4 @?= Nothing
    list5 @?= Just ([ [6, 66, 666], [6666, 66666] ], "")


testParsers :: TestTree
testParsers =
    testGroup
    "Test module Arithmetic"
    [ testCase "Parser of correct bracket sequence tests" cbsTests
    , testCase "Parser of incorrect bracket sequence tests" nonCbsTests
    , testCase "Parser of numbers tests" numberParserTests
    , testCase "Satisfy parser tests" satisfyTests
    , testCase "Element parser tests" elementTests
    , testCase "Stream parser tests" streamTests
    , testCase "Ok parser tests" okTests
    , testCase "Eof parser tests" eofTests
    , testCase "Parsing list of number tests" listParserTests
    , testCase "Parsing lists of list of number tests" listlistParserTests
    ]
