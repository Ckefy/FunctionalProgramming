module TestFS where

import Test.Tasty
import Test.Tasty.HUnit
import FSLenses
import Lens.Micro
import System.FilePath
import System.Directory

-----6th task

getDirectoryTests :: Assertion
getDirectoryTests = do
    temp <- getCurrentDirectory
    let cur = temp </> "6thTest"
    curFS <- FSLenses.getDirectory cur
    curFS @?= Dir "6thTest" [File "WinnerSurprise.txt",
                      Dir "meow" [File "Meeeeow", File "LittleKitty.txt"],
                      Dir "hello" [Dir "world" [Dir "yes" [File "brave.txt"]]]]

-- | To test lenses, I would test extra tasks which I've done
-- | Lookup block "UNNECESSARY but helpful functions from 6TH task"
-- | In FSLenses file

firstFuncTests :: Assertion
firstFuncTests = do
   firstFunc initFS @?= [Dir "D1" [], Dir "D2" [Dir "D3" [File "F1"], File "F2"]
           , Dir "D4" [Dir "D5" [Dir "D6" [File "Winner Surprise"]]]]
   firstFunc (head (initFS  ^.. cd "D2")) @?= [Dir "D3" [File "F1"]]

secondFuncTests :: Assertion
secondFuncTests = do
  secondFunc initFS @?= Just "root"
  secondFunc (File "test") @?= Nothing
  secondFunc (head (initFS  ^.. cd "D2")) @?= Just "D2"

thirdFuncTests :: Assertion
thirdFuncTests = do
  thirdFunc initFS @?= ""
  thirdFunc (File "test") @?= "test"

fourthFuncTests :: Assertion
fourthFuncTests = do
   fourthFunc initFS @?=
         Dir "/" [Dir "D1" [], Dir "D2" [Dir "D3" [File "F1"], File "F2"],
               File "F3", Dir "D4" [Dir "D5" [Dir "D6" [File "Winner Surprise"]]]]
   fourthFunc (head (initFS  ^.. cd "D1")) @?= Dir "/" []
   fourthFunc (File "justTest") @?= File "justTest"

fifthFuncTests :: Assertion
fifthFuncTests = do
   fifthFunc "-beer" initFS @?=
         Dir "root-beer" [Dir "D1" [], Dir "D2" [Dir "D3" [File "F1"], File "F2"],
               File "F3", Dir "D4" [Dir "D5" [Dir "D6" [File "Winner Surprise"]]]]
   fifthFunc "meow" (head (initFS  ^.. cd "D1")) @?= Dir "D1meow" []
   fifthFunc "meow" (File "justTest") @?= File "justTest"

sixthFuncTests :: Assertion
sixthFuncTests = do
   sixthFunc initFS @?=
            Just "D1"
   sixthFunc (head (initFS  ^.. cd "D1")) @?= Nothing
   sixthFunc (head (initFS  ^.. cd "D2")) @?= Just "D3"

seventhFuncTests :: Assertion
seventhFuncTests = do
   seventhFunc initFS @?=
            ["F3"]
   seventhFunc (head (initFS  ^.. cd "D1")) @?= []
   seventhFunc (Dir "top" [File "Best", Dir "gig" [],
                 File "FileManager", File "Ever"]) @?=
            ["Best", "FileManager", "Ever"]

-----7th task

initFS :: FS
initFS = Dir "root" [Dir "D1" [], Dir "D2" [Dir "D3" [File "F1"], File "F2"],
            File "F3", Dir "D4" [Dir "D5" [Dir "D6" [File "Winner Surprise"]]]]

cdTests :: Assertion
cdTests = do
  initFS  ^? cd "D1" @?= Just (Dir "D1" [])
  head (initFS  ^.. cd "D2") @?= Dir "D2" [Dir "D3" [File "F1"], File "F2"]
  head (head (initFS  ^.. cd "D2") ^.. cd "D3") @?= Dir "D3" [File "F1"]
  File "F3" ^? cd "D0" @?= Nothing
  initFS  ^? cd "D4" @?= Just (Dir "D4" [Dir "D5" [Dir "D6" [File "Winner Surprise"]]])
  initFS  ^? cd "D0" @?= Nothing
  head (head (head (initFS  ^.. cd "D4") ^.. cd "D5") ^.. cd "D6") @?= Dir "D6" [File "Winner Surprise"]

lsTests :: Assertion
lsTests = do
  initFS  ^.. ls @?= ["D1", "D2", "F3", "D4"]
  head (initFS  ^.. cd "D2") ^.. ls @?= ["D3", "F2"]
  head (head (initFS  ^.. cd "D2") ^.. cd "D3") ^.. ls @?= ["F1"]
  Dir "Test" [File "Tester"] ^.. ls @?= ["Tester"]
  head (head (head (initFS  ^.. cd "D4") ^.. cd "D5") ^.. cd "D6") ^.. ls @?= ["Winner Surprise"]

fileTests :: Assertion
fileTests = do
  head (head (initFS  ^.. cd "D2") ^.. cd "D3") ^? file "F1" @?= Just "F1"
  initFS ^? file "D1" @?= Nothing
  initFS ^? file "F3" @?= Just "F3"
  head (head (head (initFS  ^.. cd "D4") ^.. cd "D5") ^.. cd "D6") ^? file "Winner Surprise" @?= Just "Winner Surprise"

compRandomTests :: Assertion
compRandomTests = do
  initFS ^?  cd "D4" . cd "D5" . cd "D6" . file "Winner Surprise" @?= Just "Winner Surprise"
  initFS ^.. cd "D2" . cd "D3" . ls @?= ["F1"]
  initFS ^.. cd "D1" . ls @?= []
  initFS ^.. cd "D2" . ls @?= ["D3", "F2"]

testFS :: TestTree
testFS =
  testGroup
    "Test module File System Lenses (6th and 7th tasks)"
    [ testCase "getDirectory function from 6th task" getDirectoryTests
    , testCase "First extra function from 6th task" firstFuncTests
    , testCase "Second extra function from 6th task" secondFuncTests
    , testCase "Third extra function from 6th task" thirdFuncTests
    , testCase "Fourth extra function from 6th task" fourthFuncTests
    , testCase "Fifth extra function from 6th task" fifthFuncTests
    , testCase "Sixth extra function from 6th task" sixthFuncTests
    , testCase "Seventh extra function from 6th task" seventhFuncTests
    , testCase "CD function from 7th task" cdTests
    , testCase "LS function from 7th task" lsTests
    , testCase "FILE function from 7th task" fileTests
    , testCase "Some random tests on composition of lenses" compRandomTests
    ]