module TestNat (testNat) where

import Nat
import Test.Tasty
import Test.Tasty.HUnit

addNatTests :: Assertion
addNatTests = do
    (5 :: Nat) + (9 :: Nat) @?= (14 :: Nat)
    (9 :: Nat) + (5 :: Nat) @?= (14 :: Nat)
    (5 :: Nat) + (0 :: Nat) @?= (5 :: Nat)
    (5 :: Nat) + (55 :: Nat) @?= (60 :: Nat)

mulNatTests :: Assertion
mulNatTests = do
    (5 :: Nat) * (9 :: Nat) @?= (45 :: Nat)
    (9 :: Nat) * (5 :: Nat) @?= (45 :: Nat)
    (5 :: Nat) * (0 :: Nat) @?= (0 :: Nat)
    (0 :: Nat) * (66 :: Nat) @?= (0 :: Nat)
    (2 :: Nat) * (33 :: Nat) @?= (66 :: Nat)

subNatTests :: Assertion
subNatTests = do
    (5 :: Nat) - (9 :: Nat) @?= (0 :: Nat)
    (9 :: Nat) - (5 :: Nat) @?= (4 :: Nat)
    (5 :: Nat) - (0 :: Nat) @?= (5 :: Nat)
    (0 :: Nat) - (0 :: Nat) @?= (0 :: Nat)
    (10 :: Nat) - (5 :: Nat) @?= (5 :: Nat)

fromIntegerTests :: Assertion
fromIntegerTests = do
    fromInteger 5 @?= S (S (S (S (S Z))))
    fromInteger 4 @?= S (S (S (S Z)))
    fromInteger 3 @?= S (S (S Z))
    fromInteger 2 @?= S (S Z)
    fromInteger 1 @?= S Z
    fromInteger 0 @?= Z

natToIntegerTests :: Assertion
natToIntegerTests = do
    natToInteger (S (S (S (S (S Z))))) @?= 5
    natToInteger (S (S (S (S Z)))) @?= 4
    natToInteger (S (S (S Z))) @?= 3
    natToInteger (S (S Z)) @?= 2
    natToInteger (S Z) @?= 1
    natToInteger Z @?= 0

equalNatTests :: Assertion
equalNatTests = do
    (5 :: Nat) == (9 :: Nat) @?= False
    (3 :: Nat) == (3 :: Nat) @?= True
    (9 :: Nat) == (5 :: Nat) @?= False
    (6 :: Nat) == (6 :: Nat) @?= True
    (5 :: Nat) == (0 :: Nat) @?= False
    (0 :: Nat) == (0 :: Nat) @?= True
    (10 :: Nat) == (5 :: Nat) @?= False

ltNatTests :: Assertion
ltNatTests = do
    (5 :: Nat) < (9 :: Nat) @?= True
    (3 :: Nat) < (3 :: Nat) @?= False
    (9 :: Nat) < (5 :: Nat) @?= False
    (6 :: Nat) < (6 :: Nat) @?= False
    (0 :: Nat) < (1 :: Nat) @?= True
    (0 :: Nat) < (0 :: Nat) @?= False
    (10 :: Nat) < (55 :: Nat) @?= True

gtNatTests :: Assertion
gtNatTests = do
    (5 :: Nat) > (9 :: Nat) @?= False
    (3 :: Nat) > (3 :: Nat) @?= False
    (9 :: Nat) > (5 :: Nat) @?= True
    (6 :: Nat) > (6 :: Nat) @?= False
    (5 :: Nat) > (0 :: Nat) @?= True
    (0 :: Nat) > (0 :: Nat) @?= False
    (10 :: Nat) > (5 :: Nat) @?= True

parityTests :: Assertion
parityTests = do
    parity (0 :: Nat) @?= True
    parity (1 :: Nat) @?= False
    parity (5 :: Nat) @?= False
    parity (6 :: Nat) @?= True
    parity (9 :: Nat) @?= False
    parity (12 :: Nat) @?= True

natDivTests :: Assertion
natDivTests = do
   natDiv (10 :: Nat) (5 :: Nat) @?= (2 :: Nat)
   natDiv (16 :: Nat) (2 :: Nat) @?= (8 :: Nat)
   natDiv (9 :: Nat) (1 :: Nat) @?= (9 :: Nat)
   natDiv (27 :: Nat) (5 :: Nat) @?= (5 :: Nat)
   natDiv (28 :: Nat) (3 :: Nat) @?= (9 :: Nat)

natModTests :: Assertion
natModTests = do
   natMod (10 :: Nat) (5 :: Nat) @?= (0 :: Nat)
   natMod (16 :: Nat) (2 :: Nat) @?= (0 :: Nat)
   natMod (9 :: Nat) (2 :: Nat) @?= (1 :: Nat)
   natMod (27 :: Nat) (5 :: Nat) @?= (2 :: Nat)
   natMod (28 :: Nat) (3 :: Nat) @?= (1 :: Nat)

testNat :: TestTree
testNat =
    testGroup
    "Test module Nat"
    [ testCase "Addition function" addNatTests
    , testCase "Multiply function" mulNatTests
    , testCase "Subtraction function" subNatTests
    , testCase "fromInteger function" fromIntegerTests
    , testCase "natToInteger function" natToIntegerTests
    , testCase "Equals fuction" equalNatTests
    , testCase "Less Operator" ltNatTests
    , testCase "Greater Operator" gtNatTests
    , testCase "parity function" parityTests
    , testCase "Division function" natDivTests
    , testCase "Reminder function" natModTests
    ]