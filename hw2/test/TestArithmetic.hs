module TestArithmetic (testArithmetic) where

import Arithmetic
import Test.Tasty
import Test.Tasty.HUnit

justCalculateTests :: Assertion
justCalculateTests = do
    eval (Const 5) @?= Right 5
    eval (Add (Pow (Const 2) (Const 2)) (Mul (Const 1)(Const 10))) @?= Right 14
    eval (Sub (Div (Const 2) (Const 2)) (Mul (Const 1)(Const 10))) @?= Right (-9)
    eval (Add (Const 5) (Const 2)) @?= Right 7
    eval (Mul (Const 2) (Sub (Const 5) (Const 2))) @?= Right 6

divisionErrorTests :: Assertion
divisionErrorTests = do
    eval (Div (Const 5) (Const 0)) @?= Left ZeroDivision
    eval (Sub (Div (Const 2) (Const 0)) (Mul (Const 1)(Const 10))) @?= Left ZeroDivision
    eval (Add (Mul (Const 1)(Const 10)) (Div (Const 2) (Const 0))) @?= Left ZeroDivision
    eval (Mul (Div (Const 2) (Const 0)) (Div (Const 2) (Const 0))) @?= Left ZeroDivision

powErrorTests :: Assertion
powErrorTests = do
    eval (Pow (Const 5) (Const (-1))) @?= Left NegativePow
    eval (Mul (Pow (Const 2) (Const (-1))) (Div (Const 2) (Const 1))) @?= Left NegativePow
    eval (Mul (Div (Const 2) (Const 1)) (Pow (Const 2) (Const (-1)))) @?= Left NegativePow

testArithmetic :: TestTree
testArithmetic =
    testGroup
    "Test module Arithmetic"
    [ testCase "Just correct calculations for Arithmetic" justCalculateTests
    , testCase "Handling errors occured in div calculations for Arithmetic" divisionErrorTests
    , testCase "Handling errors occured in power calculations for Arithmetic" powErrorTests
    ]
