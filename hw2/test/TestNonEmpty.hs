module TestNonEmpty (testNonEmpty) where

import NonEmpty
import Test.Tasty
import Test.Tasty.HUnit

--functor law:
--1 fmap id = id
--2 fmap (f . g) = fmap f . fmap g

--applicative low
--1 pure id <*> v = v
--2 pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
--3 pure f <*> pure x = pure (f x)
--4 u <*> pure y = pure ($ y) <*> u

functorIdTests :: Assertion
functorIdTests = do
    let list = (1 :: Int) :| [3, 2]
    let list2 = (4 :: Int) :| []
    let list3 = (5 :: Int) :| [6]
    let list4 = (1 :: Int) :| [3, 2, 2, -1]
    let list5 = (-51 :: Int) :| [-1]
    fmap id list @?= list
    fmap id list2 @?= list2
    fmap id list3 @?= list3
    fmap id list4 @?= list4
    fmap id list5 @?= list5

functorCompositionTests :: Assertion
functorCompositionTests = do
    let list = (1 :: Int) :| [3, 2]
    let list2 = (4 :: Int) :| []
    let list3 = (5 :: Int) :| [6]
    let list4 = (1 :: Int) :| [3, 2, 2, -1]
    let list5 = (-51 :: Int) :| [-1]
    fmap ((+5) . (*100)) list @?= fmap (+5) (fmap (*100) list)
    fmap ((*10) . subtract 3) list2 @?= fmap (*10) (fmap (subtract 3) list2)
    fmap ((+5) . (+5)) list3 @?= fmap (+5) (fmap (+5) list3)
    fmap ((+5) . (*100)) list4 @?= fmap (+5) (fmap (*100) list4)
    fmap ((*10) . (*10)) list5 @?= fmap (*10) (fmap (*10) list5)

applicativeIdTests :: Assertion
applicativeIdTests = do
    let list = (1 :: Int) :| [3, 2]
    let list2 = (4 :: Int) :| []
    let list3 = (5 :: Int) :| [6]
    let list4 = (1 :: Int) :| [3, 2, 2, -1]
    let list5 = (-51 :: Int) :| [-1]
    (pure id <*> list) @?= list
    (pure id <*> list2) @?= list2
    (pure id <*> list3) @?= list3
    (pure id <*> list4) @?= list4
    (pure id <*> list5) @?= list5


--2 pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
applicativeCompositionTests :: Assertion
applicativeCompositionTests = do
    let list = (1 :: Int) :| [3, 2]
    let list2 = (4 :: Int) :| []
    let list3 = (5 :: Int) :| [6]
    let list4 = (1 :: Int) :| [3, 2, 2, -1]
    let list5 = (-51 :: Int) :| [-1]
    let u1 = fmap (*) list
    let u2 = fmap (*) list2
    let u3 = fmap (*) list3
    let v1 = fmap (+) list
    let v2 = fmap (+) list4
    let v3 = fmap (+) list5
    (pure (.) <*> u1 <*> v1 <*> list) @?= (u1 <*> (v1 <*> list))
    (pure (.) <*> u2 <*> v2 <*> list2) @?= (u2 <*> (v2 <*> list2))
    (pure (.) <*> u3 <*> v3 <*> list3) @?= (u3 <*> (v3 <*> list3))

--3 pure f <*> pure x = pure (f x)
applicativeHomoTests :: Assertion
applicativeHomoTests = do
    let f = (*5)
    let f2 = div 2
    let f3 = (-)3
    let f4 = (*10)
    let f5 = (+)100
    let pured = (pure :: (Int -> Int) -> NonEmpty (Int -> Int)) f
    let pured2 = (pure :: (Int -> Int) -> NonEmpty (Int -> Int)) f2
    let pured3 = (pure :: (Int -> Int) -> NonEmpty (Int -> Int)) f3
    let pured4 = (pure :: (Int -> Int) -> NonEmpty (Int -> Int)) f4
    let pured5 = (pure :: (Int -> Int) -> NonEmpty (Int -> Int)) f5
    let x = 6
    let x2 = 12
    let x3 = 5
    let x4 = 100
    let x5 = 666
    (pured <*> pure x) @?= pure (f x)
    (pured2 <*> pure x2) @?= pure (f2 x2)
    (pured3 <*> pure x3) @?= pure (f3 x3)
    (pured4 <*> pure x4) @?= pure (f4 x4)
    (pured5 <*> pure x5) @?= pure (f5 x5)

applicativeInterchangeTests :: Assertion
applicativeInterchangeTests = do
    let list = (1 :: Int) :| [3, 2]
    let list2 = (4 :: Int) :| []
    let list3 = (5 :: Int) :| [6]
    let list4 = (1 :: Int) :| [3, 2, 2, -1]
    let list5 = (-51 :: Int) :| [-1]
    let u = fmap (*) list
    let u2 = fmap (+) list2
    let u3 = fmap (*) list3
    let u4 = fmap (+) list4
    let u5 = fmap (*) list5
    let y = 5
    let y2 = 6
    let y3 = 3
    let y4 = 10
    let y5 = 15
    (u <*> pure y) @?= (pure ($ y) <*> u)
    (u2 <*> pure y2) @?= (pure ($ y2) <*> u2)
    (u3 <*> pure y3) @?= (pure ($ y3) <*> u3)
    (u4 <*> pure y4) @?= (pure ($ y4) <*> u4)
    (u5 <*> pure y5) @?= (pure ($ y5) <*> u5)

foldMapTests :: Assertion
foldMapTests = do
    let list = (1 :: Int) :| [3, 2]
    let list2 = (4 :: Int) :| []
    let list3 = (5 :: Int) :| [6]
    let list4 = (1 :: Int) :| [3, 2, 2, -1]
    let list5 = (-51 :: Int) :| [-1]
    foldMap (: []) list @?= [1, 3, 2]
    foldMap (: []) list2 @?= [4]
    foldMap (: []) list3 @?= [5, 6]
    foldMap (: []) list4 @?= [1, 3, 2, 2, -1]
    foldMap (: []) list5 @?= [-51, -1]

foldrTests :: Assertion
foldrTests = do
    let list = (1 :: Int) :| [3, 2]
    let list2 = (4 :: Int) :| []
    let list3 = (5 :: Int) :| [6]
    let list4 = (1 :: Int) :| [3, 2, 2, -1]
    let list5 = (-51 :: Int) :| [-1]
    foldr (mappend . (: [])) mempty list @?= [1, 3, 2]
    foldr (mappend . (: [])) mempty list2 @?= [4]
    foldr (mappend . (: [])) mempty list3 @?= [5, 6]
    foldr (mappend . (: [])) mempty list4 @?= [1, 3, 2, 2, -1]
    foldr (mappend . (: [])) mempty list5 @?= [-51, -1]

testNonEmpty:: TestTree
testNonEmpty =
    testGroup
    "Test module NonEmpty"
    [ testCase "Functor law of id for NonEmpty" functorIdTests
    , testCase "Functor law of composition for NonEmpty" functorCompositionTests
    , testCase "Applicative law of id for NonEmpty" applicativeIdTests
    , testCase "Applicative law of Composition for NonEmpty" applicativeCompositionTests
    , testCase "Applicative law of Homomorphism for NonEmpty" applicativeHomoTests
    , testCase "Applicative law of Interchange for NonEmpty" applicativeInterchangeTests
    , testCase "FoldMap tests for NonEmpty" foldMapTests
    , testCase "Foldr tests for NonEmpty" foldrTests
    ]
