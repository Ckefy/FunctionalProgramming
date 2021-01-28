{-# LANGUAGE OverloadedStrings #-}

module TestsFile where

import Test.Tasty
import Test.Tasty.HUnit
import Shell
import FileSystemOperations
import Parser
import FileSystemTypes
import Data.ByteString.Char8 (ByteString)
import Control.Monad.Writer.Strict (runWriterT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Lazy (runState)
import Control.Monad.Except (runExceptT)
import System.Directory (getCurrentDirectory)
import System.FilePath (combine, splitDirectories, dropFileName)
import Data.Map.Strict ((!), (!?))
import Data.Time.Clock.POSIX (getCurrentTime)
import Control.Monad (void)

directCheck :: Bool -> FilePath -> CondShell -> Assertion
directCheck flag path shell = do
  let rootShell = root shell
  let abs = concate rootShell ((reverse . splitDirectories) path)
  if flag then
    (case mapDirs shell !? abs of
         Just _ -> return ()
         Nothing -> assertFailure ("No such direct with this name " ++ path))
    else
    (case mapDirs shell !? abs of
         Just _ -> assertFailure ("Direct with such name exist " ++ path)
         Nothing -> return ())

fileCheck :: Bool -> FilePath -> CondShell -> Assertion
fileCheck flag path shell = do
  let rootShell = root shell
  let (name:parDir) = concate rootShell ((reverse . splitDirectories) path)
  if flag then
    (case mapDirs shell !? parDir of
         Nothing -> assertFailure $ "No such direct with this name " ++ dropFileName path
         Just dir ->
               case filesInsideDir dir !? name of
                 Nothing -> assertFailure $ "No such file with this name" ++ path
                 Just _  -> return ())
    else
    (case mapDirs shell !? parDir of
         Nothing -> return ()
         Just dir ->
           case filesInsideDir dir !? name of
             Nothing -> return ()
             Just _  -> assertFailure $ "File with such name exist " ++ path)

initialize :: IO CondShell
initialize = do
  dir <- getCurrentDirectory
  makeShell (combine dir "virtual")

doCheck com shell expected func = do
  time <- getCurrentTime
  let ((res, real), newState) = runState (runWriterT (runReaderT (runExceptT $ makeCommand com) time)) shell
  case res of
     Left err -> assertFailure (show err)
     Right _ -> return () 
  real @?= expected
  func newState
  return newState

doCheckIgnore = (((void .) .) .) . doCheck

cd shell path = do
  let cur = currentPath shell
  let fullpath = concate cur ((reverse . splitDirectories) path)
  shell {currentPath = fullpath, currentDirect = mapDirs shell ! fullpath}

cdTest :: Assertion
cdTest = do
  shell <- initialize
  newShell <- doCheck (Cd "folderCD") shell "" (@?= cd shell "folderCD")
  newShell2 <- doCheck (Cd "folderCDD") newShell "" (@?= cd shell "folderCD/folderCDD")
  doCheckIgnore (Cd ".") newShell2 "" (@?= newShell2)
  newShell3 <- doCheck (Cd "..") newShell2 "" (@?= cd shell "folderCD")
  newShell4 <- doCheck (Cd ".") newShell3 "" (@?= cd shell "folderCD")
  doCheckIgnore (Cd "..") newShell4 "" (@?= shell)

mkFileTest :: Assertion
mkFileTest = do
  shell <- initialize
  doCheckIgnore (MakeFile "test1R.txt") shell "" (fileCheck True "test1R.txt")
  doCheckIgnore (MakeFile "test2R") shell "" (fileCheck True "test2R")
  doCheckIgnore (MakeFile "folder/test3R.another") shell "" (fileCheck True "folder/test3R.another")
  let newShell = cd shell "folderCD"
  doCheckIgnore (MakeFile "../folder/test3R.another") newShell "" (fileCheck True "folder/test3R.another")
  doCheckIgnore (MakeFile "test4R.txt") newShell "" (fileCheck True "folderCD/test4R.txt")
  doCheckIgnore (MakeFile "./test4R.txt") newShell "" (fileCheck True "folderCD/test4R.txt")
  let newShell2 = cd shell "folderCD/folderCDD"
  doCheckIgnore (MakeFile "./prizeAnother.txt") newShell2 "" (fileCheck True "folderCD/folderCDD/prizeAnother.txt")

delFileTest :: Assertion
delFileTest = do
  shell <- initialize
  new1 <- doCheck (MakeFile "test1R.txt") shell "" (fileCheck True "test1R.txt")
  new2 <- doCheck (DelFile "test1R.txt") new1 "" (fileCheck False "test1R.txt")
  new3 <- doCheck (MakeFile "folder/test3R.another") new2 "" (fileCheck True "folder/test3R.another")
  let newShell = cd shell "folderCD"
  newNew1 <- doCheck (MakeFile "../folder/test3R.another") newShell "" (fileCheck True "folder/test3R.another")
  newNew2 <- doCheck (MakeFile "test4R.txt") newNew1 "" (fileCheck True "folderCD/test4R.txt")
  newNew3 <- doCheck (DelFile "./test4R.txt") newNew2 "" (fileCheck False "folderCD/test4R.txt")
  let newShell2 = cd shell "folderCD/folderCDD"
  newNewNew1 <- doCheck (MakeFile "./prizeAnother.txt") newShell2 "" (fileCheck True "folderCD/folderCDD/prizeAnother.txt")
  newNewNew2 <- doCheck (DelFile "./prizeAnother.txt") newNewNew1 "" (fileCheck False "folderCD/folderCDD/prizeAnother.txt")
  newNewNew3 <- doCheck (MakeFile "./prizeAnother.txt") newNewNew2 "" (fileCheck True "folderCD/folderCDD/prizeAnother.txt")
  newNewNew4 <- doCheck (DelFile "./prizeAnother.txt") newNewNew3 "" (fileCheck False "folderCD/folderCDD/prizeAnother.txt")
  doCheckIgnore (MakeFile "./prizeAnother.txt") newNewNew4 "" (fileCheck True "folderCD/folderCDD/prizeAnother.txt")

mkDirTest :: Assertion
mkDirTest = do
  shell <- initialize
  doCheckIgnore (MakeDir "test1R") shell "" (directCheck True "test1R")
  doCheckIgnore (MakeDir "test2R") shell "" (directCheck True "test2R")
  doCheckIgnore (MakeDir "folder/test3R") shell "" (directCheck True "folder/test3R")
  let newShell = cd shell "folderCD"
  doCheckIgnore (MakeDir "../folder/test3R") newShell "" (directCheck True "folder/test3R")
  doCheckIgnore (MakeDir "test4R") newShell "" (directCheck True "folderCD/test4R")
  doCheckIgnore (MakeDir "./test4R") newShell "" (directCheck True "folderCD/test4R")
  let newShell2 = cd shell "folderCD/folderCDD"
  doCheckIgnore (MakeDir "./prizeFolder") newShell2 "" (directCheck True "folderCD/folderCDD/prizeFolder")

delDirTest :: Assertion
delDirTest = do
  shell <- initialize
  new1 <- doCheck (MakeDir "test1R") shell "" (directCheck True "test1R")
  new2 <- doCheck (DelDir "test1R") new1 "" (directCheck False "test1R")
  new3 <- doCheck (MakeDir "folder/test3R") new2 "" (directCheck True "folder/test3R")
  let newShell = cd shell "folderCD"
  newNew1 <- doCheck (MakeDir "../folder/test3R") newShell "" (directCheck True "folder/test3R")
  newNew2 <- doCheck (MakeDir "test4R") newNew1 "" (directCheck True "folderCD/test4R")
  newNew3 <- doCheck (DelDir "./test4R") newNew2 "" (directCheck False "folderCD/test4R")
  let newShell2 = cd shell "folderCD/folderCDD"
  newNewNew1 <- doCheck (MakeDir "./prizeAnother") newShell2 "" (directCheck True "folderCD/folderCDD/prizeAnother")
  newNewNew2 <- doCheck (DelDir "./prizeAnother") newNewNew1 "" (directCheck False "folderCD/folderCDD/prizeAnother")
  newNewNew3 <- doCheck (MakeDir "./prizeAnother") newNewNew2 "" (directCheck True "folderCD/folderCDD/prizeAnother")
  newNewNew4 <- doCheck (DelDir "./prizeAnother") newNewNew3 "" (directCheck False "folderCD/folderCDD/prizeAnother")
  doCheckIgnore (MakeDir "./prizeAnother") newNewNew4 "" (directCheck True "folderCD/folderCDD/prizeAnother")

catTest :: Assertion
catTest = do
  shell <- initialize
  doCheckIgnore (Cat "test1.txt") shell "kek" (@?= shell)
  doCheckIgnore (Cat "./test2") shell "lel" (@?= shell)
  let newShell1 = cd shell "folderCD"
  doCheckIgnore (Cat "test4.txt") newShell1 "Holy cow\r\nits two rows" (@?= newShell1)
  doCheckIgnore (Cat "../test2") newShell1 "lel" (@?= newShell1)
  let newShell2 = cd shell "folderCD/folderCDD"
  doCheckIgnore (Cat "prize.txt") newShell2 "Yes, you won!" (@?= newShell2)

writeFileTest :: Assertion
writeFileTest = do
  shell <- initialize
  doCheckIgnore (Cat "test1.txt") shell "kek" (@?= shell)
  ashell <- doCheck (WriteFile "test1.txt" "omg") shell "" (const $ return ())
  doCheckIgnore (Cat "test1.txt") ashell "omg" (@?= ashell)
  let newShell1 = cd shell "folderCD"
  doCheckIgnore (Cat "test4.txt") newShell1 "Holy cow\r\nits two rows" (@?= newShell1)
  ashell2 <- doCheck (WriteFile "test4.txt" "Hello") newShell1 "" (const $ return ())
  doCheckIgnore (Cat "test4.txt") ashell2 "Hello" (@?= ashell2)

contWriteFileTest :: Assertion
contWriteFileTest = do
  shell <- initialize
  doCheckIgnore (Cat "test1.txt") shell "kek" (@?= shell)
  ashell <- doCheck (ContWriteFile "test1.txt" "omg") shell "" (const $ return ())
  doCheckIgnore (Cat "test1.txt") ashell "kekomg" (@?= ashell)
  let newShell1 = cd shell "folderCD"
  doCheckIgnore (Cat "test4.txt") newShell1 "Holy cow\r\nits two rows" (@?= newShell1)
  ashell2 <- doCheck (ContWriteFile "test4.txt" "Hello") newShell1 "" (const $ return ())
  doCheckIgnore (Cat "test4.txt") ashell2 "Holy cow\r\nits two rowsHello" (@?= ashell2)

findTest :: Assertion
findTest = do
  shell <- initialize
  doCheckIgnore (FindFile "" "test1.txt") shell "\\test1.txt\n" (@?= shell)
  doCheckIgnore (FindFile "folder" "test3.another") shell "folder\\test3.another\n" (@?= shell)
  doCheckIgnore (FindFile "folder" "smislzhizni.txt") shell "" (@?= shell)
  newShell <- doCheck (MakeFile "folderCD/smislzhizni.txt") shell "" (fileCheck True "folderCD/smislzhizni.txt")
  doCheckIgnore (FindFile "folderCD" "smislzhizni.txt") newShell "folderCD\\smislzhizni.txt\n" (@?= newShell)
  doCheckIgnore (FindFile "" "test1.txt") newShell "\\test1.txt\n" (@?= newShell)

lsCurTest :: Assertion
lsCurTest = do
   shell <- initialize
   doCheckIgnore LsCur shell "DIRECTORIES:\nfolder\nfolderCD\nFILES:\ntest1.txt\ntest2\n" (@?= shell)
   let newShell1 = cd shell "folderCD"
   doCheckIgnore LsCur newShell1 "DIRECTORIES:\nfolderCDD\nFILES:\ntest4.txt\n" (@?= newShell1)

lsTest :: Assertion
lsTest = do
   shell <- initialize
   doCheckIgnore (Ls ".") shell "DIRECTORIES:\nfolder\nfolderCD\nFILES:\ntest1.txt\ntest2\n" (@?= shell)
   let newShell1 = cd shell "folderCD"
   doCheckIgnore (Ls ".") newShell1 "DIRECTORIES:\nfolderCDD\nFILES:\ntest4.txt\n" (@?= newShell1)
   doCheckIgnore (Ls "..") newShell1 "DIRECTORIES:\nfolder\nfolderCD\nFILES:\ntest1.txt\ntest2\n" (@?= newShell1)

myTests :: TestTree
myTests =
   testGroup
     "Tests begin"
     [testCase "Tests of making file" mkFileTest
    , testCase "Tests of making direct" mkDirTest
    , testCase "Tests of changing direct" cdTest
    , testCase "Tests of showing file" catTest
    , testCase "Tests of deleting file" delFileTest
    , testCase "Tests of deleting direct" delDirTest
    , testCase "Tests of writing to file" writeFileTest
    , testCase "Tests of continue writing to file" contWriteFileTest
    , testCase "Tests of finding file" findTest
    , testCase "Tests of displaying current directory" lsCurTest
    , testCase "Tests of displaying given directory" lsTest ]