module TestGeom where

import GeometryNaive as N
import GeometryParallel as P
import Test.Tasty
import Test.Tasty.HUnit

parallelPerimeterTests :: Assertion
parallelPerimeterTests = do
    P.perimeter [P.Point 0 0, P.Point 1 0, P.Point 1 1, P.Point 0 1] @?= 4
    P.perimeter [P.Point 0 0, P.Point 0 0, P.Point 0 0, P.Point 0 0] @?= 0
    P.perimeter [P.Point 0 0, P.Point 1 0, P.Point 1 0, P.Point 0 0] @?= 2
    P.perimeter [P.Point 0 0, P.Point 3 4] @?= 10
    P.perimeter [P.Point 100500 100500] @?= 0
    P.perimeter [P.Point 5 5, P.Point 6 5, P.Point 6 6, P.Point 7 6, P.Point 7 7
                     , P.Point 7 6, P.Point 6 6, P.Point 6 5] @?= 8

naivePerimeterTests :: Assertion
naivePerimeterTests = do
    N.perimeter [N.Point 0 0, N.Point 1 0, N.Point 1 1, N.Point 0 1] @?= 4
    N.perimeter [N.Point 0 0, N.Point 0 0, N.Point 0 0, N.Point 0 0] @?= 0
    N.perimeter [N.Point 0 0, N.Point 1 0, N.Point 1 0, N.Point 0 0] @?= 2
    N.perimeter [N.Point 0 0, N.Point 3 4] @?= 10
    N.perimeter [N.Point 100500 100500] @?= 0
    N.perimeter [N.Point 5 5, N.Point 6 5, N.Point 6 6, N.Point 7 6, N.Point 7 7
                     , N.Point 7 6, N.Point 6 6, N.Point 6 5] @?= 8

parallelSquareTests :: Assertion
parallelSquareTests = do
    P.doubleArea [P.Point 0 0, P.Point 1 0, P.Point 1 1, P.Point 0 1] @?= 2
    P.doubleArea [P.Point 0 0, P.Point 0 0, P.Point 0 0, P.Point 0 0] @?= 0
    P.doubleArea [P.Point 0 0, P.Point 1 0, P.Point 1 0, P.Point 0 0] @?= 0
    P.doubleArea [P.Point 0 0, P.Point 3 4, P.Point 5 5] @?= 5
    P.doubleArea [P.Point 100500 100500] @?= 0
    P.doubleArea [P.Point 5 5, P.Point 6 5, P.Point 6 6, P.Point 7 6, P.Point 7 7
                     , P.Point 8 7, P.Point 8 4, P.Point 6 4] @?= 11

naiveSquareTests :: Assertion
naiveSquareTests = do
    N.doubleArea [N.Point 0 0, N.Point 1 0, N.Point 1 1, N.Point 0 1] @?= 2
    N.doubleArea [N.Point 0 0, N.Point 0 0, N.Point 0 0, N.Point 0 0] @?= 0
    N.doubleArea [N.Point 0 0, N.Point 1 0, N.Point 1 0, N.Point 0 0] @?= 0
    N.doubleArea [N.Point 0 0, N.Point 3 4, N.Point 5 5] @?= 5
    N.doubleArea [N.Point 100500 100500] @?= 0
    N.doubleArea [N.Point 5 5, N.Point 6 5, N.Point 6 6, N.Point 7 6, N.Point 7 7
                     , N.Point 8 7, N.Point 8 4, N.Point 6 4] @?= 11

allPlusTests :: Assertion
allPlusTests = do
    P.plus (P.Point 0 0) (P.Point 1 0) @?= P.Point 1 0
    P.plus (P.Point 0 0) (P.Point 0 0) @?= P.Point 0 0
    P.plus (P.Point 3 4) (P.Point 5 5) @?= P.Point 8 9
    P.plus (P.Point 100500 100500) (P.Point 1500 1500) @?= P.Point 102000 102000

allMinusTests :: Assertion
allMinusTests = do
    P.minus (P.Point 0 0) (P.Point 1 0) @?= P.Point (-1) 0
    P.minus (P.Point 0 0) (P.Point 0 0) @?= P.Point 0 0
    P.minus (P.Point 3 4) (P.Point 5 5) @?= P.Point (-2) (-1)
    P.minus (P.Point 100500 100500) (P.Point 0 0) @?= P.Point 100500 100500

allScalarTests :: Assertion
allScalarTests = do
    P.scalarProduct (P.Point 1 0) (P.Point 1 0) @?= 1
    P.scalarProduct (P.Point 0 0) (P.Point 0 0) @?= 0
    P.scalarProduct (P.Point 3 4) (P.Point 5 5) @?= 35
    P.scalarProduct (P.Point 100500 100500) (P.Point 10 10) @?= 2010000

allCrossTests :: Assertion
allCrossTests = do
    P.crossProduct (P.Point 2 0) (P.Point 1 1) @?= 2
    P.crossProduct (P.Point 0 0) (P.Point 0 0) @?= 0
    P.crossProduct (P.Point 4 3) (P.Point 5 5) @?= 5
    P.crossProduct (P.Point 100500 100500) (P.Point 0 10) @?= 1005000

testGeom :: TestTree
testGeom =
    testGroup
    "Test module Geometry (1st task)"
    [testCase "Parallel perimeter" parallelPerimeterTests
   , testCase "Naive perimeter" naivePerimeterTests
   , testCase "Parallel double square" parallelSquareTests
   , testCase "Naive double square" naiveSquareTests
   , testCase  "Plus function" allPlusTests
   , testCase "Minus function" allMinusTests
   , testCase "Scalar product function" allScalarTests
   , testCase "Cross product function" allCrossTests
   ]

