module TestPlants (testPlants) where

import Plants
import Test.Tasty
import Test.Tasty.HUnit
import Data.List.NonEmpty

emptyTests :: Assertion
emptyTests = do
    empty (Node List List (1 :| [])) @?= False
    empty (Node (Node List List (2 :| [2])) List (3 :| [])) @?= False
    empty List @?= True

sizeTests :: Assertion
sizeTests = do
    size (Node List List (1 :| [])) @?= 1
    size (Node (Node List List (2 :| [2])) List (3 :| [])) @?= 3
    size List @?= 0
    size (Node (Node List List (2 :| [])) List (3 :| [])) @?= 2

findTests :: Assertion
findTests = do
    let tree = (Node (Node List List (2 :| [2])) (Node List List (5 :| [])) (3 :| []))
    find 5 tree @?= Just 5
    find 2 tree @?= Just 2
    find 0 tree @?= Nothing
    find 11 tree @?= Nothing
    find 3 tree @?= Just 3

addTreeTests :: Assertion
addTreeTests = do
   let tree = (Node (Node List List (2 :| [2])) (Node List List (5 :| [])) (3 :| []))
   let treeAns1 = Node
            (Node List List (2 :| [2, 2]))
              (Node List List (5 :| [])) (3 :| [])
   let treeAns2 = Node
               (Node List List (2 :| [2]))
                 (Node List (Node List List (6 :| [])) (5 :| [])) (3 :| [])
   let treeAns3 = Node
               (Node List List (2 :| [2]))
                 (Node List List (5 :| [])) (3 :| [3])
   add 2 tree @?= treeAns1
   add 6 tree @?= treeAns2
   add 3 tree @?= treeAns3

makeTreeTests :: Assertion
makeTreeTests = do
    let list1 = [1, 1, 1, 2, 3, 1, 4, 5, 7]
    let list2 = [200]
    let tree1 = Node
                 (Node (Node List List (1 :| [1,1,1]))
                   (Node List List (3 :| [])) (2 :| []))
                   (Node (Node List List (5 :| [])) List (7 :| [])) (4 :| [])
    let tree2 = Node List List (200 :| [])
    makeTree list1 @?= tree1
    makeTree list2 @?= tree2

deleteTreeTests :: Assertion
deleteTreeTests = do
   let tree = (Node (Node List List (2 :| [2])) (Node List List (5 :| [])) (3 :| []))
   let tree1 = Node (Node List List (2 :| [2])) List (5 :| [])
   let tree2 = (Node (Node List List (2 :| [])) (Node List List (5 :| [])) (3 :| []))
   let tree3 = (Node (Node List List (2 :| [2])) List (3 :| []))
   deleteTree 3 tree @?= tree1
   deleteTree 2 tree @?= tree2
   deleteTree 5 tree @?= tree3
   deleteTree 1 tree @?= tree

testPlants :: TestTree
testPlants =
    testGroup
    "Test module Plants"
    [ testCase "Checking of empty function" emptyTests
    , testCase "Checking of size function" sizeTests
    , testCase "searching function" findTests
    , testCase "inserting function" addTreeTests
    , testCase "fromList function" makeTreeTests
    , testCase "deleting function" deleteTreeTests
    ]