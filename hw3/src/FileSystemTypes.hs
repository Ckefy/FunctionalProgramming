module FileSystemTypes where
import System.Directory (Permissions)
import Data.ByteString.Char8 (ByteString)
import Data.Time.Clock (UTCTime)
import Data.Map.Strict (Map)
import System.FilePath (joinPath, splitDirectories)

type Name = String --like name of dir, files and etc
type Path = [Name] --something/another/file

makeFilePath :: Path -> FilePath
makeFilePath = joinPath . reverse

concate1 beforePath insidePath@("/":xs) = reverse insidePath
concate1 beforePath [] = beforePath
concate1 (beforePath:xs1) ("..":xs2) = concate1 xs1 xs2
concate1 beforePath (".":xs) = concate1 beforePath xs
concate1 beforePath (x:xs) = concate1 (x : beforePath) xs

concate :: Path -> Path -> Path
concate beforePath insidePath = concate1 beforePath $ reverse insidePath

data File = File {
    nameFile :: Name
  , sizeFile :: Integer
  , permFile :: Permissions
  , pathFile :: Path --something/another/
  , contFile :: ByteString
  , modiFile :: UTCTime
} deriving (Eq)

data Direct = Direct {
    nameDir :: Name
  , sizeDir :: Integer
  , permDir :: Permissions
  , pathDir :: Path
  , modiDir :: UTCTime
  , filesInsideDir :: Map Name File
  , dirsInsideDir :: Map Name Path
} deriving (Eq, Show)

data Command =
   MakeFile FilePath --to make empty file
 | Cd FilePath    --to go to another dir
 | Cat FilePath   --to show file
 | DelDir FilePath
 | DelFile FilePath
 | WriteFile FilePath ByteString
 | ContWriteFile FilePath ByteString
 | MakeDir FilePath
 | FindFile FilePath Name
 | LsCur
 | Ls FilePath
 | Inform FilePath
 | CurLoc
 | Save
 | Exit

data Errors =
   ErrorMakeFile Path String
 | ErrorNoPath Path String --when there is no such path
 | ErrorNoFile Path String
 | ErrorNoDir Path String
 | ErrorDelDir Path String
 | ErrorDelFile Path String
 | ErrorMakeDir Path String
 | ErrorFind Path String
 | ErrorWrite Path String

instance Show File where
   show (File name size perm path cont modi) = makeFilePath (name : path) ++ " " ++ show modi ++ " " ++ show size ++ " " ++ show perm

instance Show Errors where
   show (ErrorMakeFile path str) = "Error occured during making file " ++ makeFilePath path ++ " " ++ str
   show (ErrorNoPath path str) = "No such path " ++ makeFilePath path ++ " " ++ str
   show (ErrorNoFile path str) = "No such file " ++ makeFilePath path ++ " " ++ str
   show (ErrorNoDir path str) = "No such folder " ++ makeFilePath path ++ " " ++ str
   show (ErrorDelDir path str) = "Error occured during deleting folder " ++ makeFilePath path ++ " " ++ str
   show (ErrorDelFile path str) = "Error occured during deleting file " ++ makeFilePath path ++ " " ++ str
   show (ErrorMakeDir path str) = "Error occured during making folder " ++ makeFilePath path ++ " " ++ str
   show (ErrorWrite path str) = "Error occured during writing to file " ++ makeFilePath path ++ " " ++ str