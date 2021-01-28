module TestFoldable (testFoldable) where

import Plants
import Test.Tasty
import Test.Tasty.HUnit
import Data.Foldable (toList)
import Data.List (sort)
import Data.List.NonEmpty hiding (toList, sort)

compToFromTests :: Assertion
compToFromTests = do
    let list1 = [1, 2, 3, 4, 5]
    let list2 = [100]
    let list3 = [5, 5, 1, 2, 5, 3, 2, 5, 6, 7, 4]
    let list4 = [200, 200, 200, 200, 200, 200]
    toList (makeTree list1) @?= sort list1
    toList (makeTree list2) @?= sort list2
    toList (makeTree list3) @?= sort list3
    toList (makeTree list4) @?= sort list4

foldrTests :: Assertion
foldrTests = do
    let tree = (Node (Node List List (2 :| [2])) (Node List List (5 :| [])) (3 :| []))
    let tree1 = Node (Node List List (2 :| [2])) List (5 :| [])
    let tree2 = (Node (Node List List (2 :| [])) (Node List List (5 :| [])) (3 :| []))
    let tree3 = (Node (Node List List (2 :| [2])) List (3 :| []))
    foldr (mappend . (: [])) mempty tree @?= [2, 2, 3, 5]
    foldr (mappend . (: [])) mempty tree1 @?= [2, 2, 5]
    foldr (mappend . (: [])) mempty tree2 @?= [2, 3, 5]
    foldr (mappend . (: [])) mempty tree3 @?= [2, 2, 3]

foldMapTests :: Assertion
foldMapTests = do
    let tree = (Node (Node List List (2 :| [2])) (Node List List (5 :| [])) (3 :| []))
    let tree1 = Node (Node List List (2 :| [2])) List (5 :| [])
    let tree2 = (Node (Node List List (2 :| [])) (Node List List (5 :| [])) (3 :| []))
    let tree3 = (Node (Node List List (2 :| [2])) List (3 :| []))
    foldMap (: []) tree @?= [2, 2, 3, 5]
    foldMap (: []) tree1 @?= [2, 2, 5]
    foldMap (: []) tree2 @?= [2, 3, 5]
    foldMap (: []) tree3 @?= [2, 2, 3]

testFoldable :: TestTree
testFoldable =
    testGroup
    "Test module Foldable"
    [ testCase "Composition of toList and fromList" compToFromTests
    , testCase "foldr function" foldrTests
    , testCase "foldMap function" foldMapTests
    ]