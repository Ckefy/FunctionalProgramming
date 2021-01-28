module TestTreeInst (testTreeInst) where

import TreeInst
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
    let tree = Leaf 5
    let tree2 = Branch (Leaf 5) (Branch (Leaf 1) (Leaf 2))
    let tree3 = Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 1) (Leaf 2))
    let tree4 = Branch (Leaf 555) (Leaf 222)
    let tree5 = Branch (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 2)) (Branch (Leaf 1) (Leaf 2))
    fmap id tree @?= tree
    fmap id tree2 @?= tree2
    fmap id tree3 @?= tree3
    fmap id tree4 @?= tree4
    fmap id tree5 @?= tree5

functorCompositionTests :: Assertion
functorCompositionTests = do
    let tree = Leaf 5
    let tree2 = Branch (Leaf 5) (Branch (Leaf 1) (Leaf 2))
    let tree3 = Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 1) (Leaf 2))
    let tree4 = Branch (Leaf 555) (Leaf 222)
    let tree5 = Branch (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 2)) (Branch (Leaf 1) (Leaf 2))
    fmap ((+5) . (*100)) tree @?= fmap (+5) (fmap (*100) tree)
    fmap ((*10) . subtract 3) tree2 @?= fmap (*10) (fmap (subtract 3) tree2)
    fmap ((+5) . (+5)) tree3 @?= fmap (+5) (fmap (+5) tree3)
    fmap ((+5) . (*100)) tree4 @?= fmap (+5) (fmap (*100) tree4)
    fmap ((*10) . (*10)) tree5 @?= fmap (*10) (fmap (*10) tree5)

applicativeIdTests :: Assertion
applicativeIdTests = do
    let tree = Leaf 5
    let tree2 = Branch (Leaf 5) (Branch (Leaf 1) (Leaf 2))
    let tree3 = Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 1) (Leaf 2))
    let tree4 = Branch (Leaf 555) (Leaf 222)
    let tree5 = Branch (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 2)) (Branch (Leaf 1) (Leaf 2))
    (pure id <*> tree) @?= tree
    (pure id <*> tree2) @?= tree2
    (pure id <*> tree3) @?= tree3
    (pure id <*> tree4) @?= tree4
    (pure id <*> tree5) @?= tree5


--2 pure (.) <*> u <*> v <*> w = u <*> (v <*> w)s
applicativeCompositionTests :: Assertion
applicativeCompositionTests = do
    let tree = Leaf 5
    let tree2 = Branch (Leaf 5) (Branch (Leaf 1) (Leaf 2))
    let tree3 = Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 1) (Leaf 2))
    let tree4 = Branch (Leaf 555) (Leaf 222)
    let tree5 = Branch (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 2)) (Branch (Leaf 1) (Leaf 2))
    let u1 = fmap (*) tree
    let u2 = fmap (*) tree2
    let u3 = fmap (*) tree3
    let v1 = fmap (+) tree
    let v2 = fmap (+) tree4
    let v3 = fmap (+) tree5
    (pure (.) <*> u1 <*> v1 <*> tree) @?= (u1 <*> (v1 <*> tree))
    (pure (.) <*> u2 <*> v2 <*> tree2) @?= (u2 <*> (v2 <*> tree2))
    (pure (.) <*> u3 <*> v3 <*> tree3) @?= (u3 <*> (v3 <*> tree3))

--3 pure f <*> pure x = pure (f x)
applicativeHomoTests :: Assertion
applicativeHomoTests = do
    let tree = Leaf 5
    let tree2 = Branch (Leaf 5) (Branch (Leaf 1) (Leaf 2))
    let tree3 = Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 1) (Leaf 2))
    let tree4 = Branch (Leaf 555) (Leaf 222)
    let tree5 = Branch (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 2)) (Branch (Leaf 1) (Leaf 2))
    tree @?= tree

applicativeInterchangeTests :: Assertion
applicativeInterchangeTests = do
    let f = (*5)
    let f2 = div 2
    let f3 = (-)3
    let f4 = (*10)
    let f5 = (+)100
    let pured = (pure :: (Int -> Int) -> Tree (Int -> Int)) f
    let pured2 = (pure :: (Int -> Int) -> Tree (Int -> Int)) f2
    let pured3 = (pure :: (Int -> Int) -> Tree (Int -> Int)) f3
    let pured4 = (pure :: (Int -> Int) -> Tree (Int -> Int)) f4
    let pured5 = (pure :: (Int -> Int) -> Tree (Int -> Int)) f5
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

foldMapTests :: Assertion
foldMapTests = do
    let tree = Leaf 5
    let tree2 = Branch (Leaf 5) (Branch (Leaf 1) (Leaf 2))
    let tree3 = Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 1) (Leaf 2))
    let tree4 = Branch (Leaf 555) (Leaf 222)
    let tree5 = Branch (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 2)) (Branch (Leaf 1) (Leaf 2))
    foldMap (: []) tree @?= [5]
    foldMap (: []) tree2 @?= [5, 1, 2]
    foldMap (: []) tree3 @?= [1, 2, 1, 2]
    foldMap (: []) tree4 @?= [555, 222]
    foldMap (: []) tree5 @?= [1, 2, 2, 1, 2]

foldrTests :: Assertion
foldrTests = do
    let tree = Leaf 5
    let tree2 = Branch (Leaf 5) (Branch (Leaf 1) (Leaf 2))
    let tree3 = Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 1) (Leaf 2))
    let tree4 = Branch (Leaf 555) (Leaf 222)
    let tree5 = Branch (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 2)) (Branch (Leaf 1) (Leaf 2))
    foldr (mappend . (: [])) mempty tree @?= [5]
    foldr (mappend . (: [])) mempty tree2 @?= [5, 1, 2]
    foldr (mappend . (: [])) mempty tree3 @?= [1, 2, 1, 2]
    foldr (mappend . (: [])) mempty tree4 @?= [555, 222]
    foldr (mappend . (: [])) mempty tree5 @?= [1, 2, 2, 1, 2]

testTreeInst:: TestTree
testTreeInst =
    testGroup
    "Test module TreeInst"
    [ testCase "Functor law of id for Tree" functorIdTests
    , testCase "Functor law of composition for Tree" functorCompositionTests
    , testCase "Applicative law of id for Tree" applicativeIdTests
    , testCase "Applicative law of Composition for Tree" applicativeCompositionTests
    , testCase "Applicative law of Homomorphism for Tree" applicativeHomoTests
    , testCase "Applicative law of Interchange for Tree" applicativeInterchangeTests
    , testCase "FoldMap tests for Tree" foldMapTests
    , testCase "Foldr tests for Tree" foldrTests
    ]
