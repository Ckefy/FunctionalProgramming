module Main where

import Lib
import Shell
import FileSystemTypes
import Parser
import FileSystemOperations

import System.IO (getLine, stdin, stdout)
import qualified Data.ByteString.Char8 as B8 (putStrLn, length)
import Control.Exception.Safe as CES (onException)
import System.Directory
import System.Directory.Internal.Prelude
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (runState)
import Control.Monad.Writer.Strict
import Data.Char (isSpace)
import Data.Time (getCurrentTime)
import Debug.Trace (trace)
import Options.Applicative.Extra

main :: IO ()
main = do
  putStrLn "Hello! Please wait..."
  hFlush stdout
  rootPath <- getCurrentDirectory
  fileSystem <- makeShell rootPath
  putStrLn "I'm ready, sir! You can use --help to learn more about my abilities" >> hFlush stdout
  CES.onException (next fileSystem) (saveMain fileSystem)
    
next :: CondShell -> IO ()
next shell = do
  unparsedCommand <- getLine
  command <- handleError $ parseMain unparsedCommand
  case command of
    Just Exit -> do
      putStrLn "Saving all changes before exiting, please wait.."
      hFlush stdout
      saveMain shell
      putStrLn "Good bye and have a nice day!"
      hFlush stdout
    Just Save -> do
      putStrLn "Saving, please wait.."
      hFlush stdout
      saveMain shell
      putStrLn "All changes saved, you can now continue your work!"
      hFlush stdout
      CES.onException (next shell) (saveMain shell)
    Just p -> do
      time <- getCurrentTime
      let ((res, out), newShell) =
            runState (runWriterT (runReaderT (runExceptT $ makeCommand p) time)) shell
      case res of
        Left e -> print e
        Right _ -> if B8.length out == 0 then
                     do putStrLn "Work's done, all changes will be displayed after saving"
                        hFlush stdout
                     else
                     do B8.putStrLn out
                        hFlush stdout
      CES.onException (next newShell) (saveMain newShell)
    Nothing -> do
      hFlush stdout
      CES.onException (next shell) (saveMain shell)

handleError :: ParserResult a -> IO (Maybe a)
handleError res = case res of
  (Failure failure) -> do
     let (text, el) = renderFailure failure ""
     putStrLn text
     return Nothing
  _ -> Just <$> handleParseResult res