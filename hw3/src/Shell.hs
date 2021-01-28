{-# LANGUAGE OverloadedStrings #-}
module Shell where

import FileSystemTypes
import FileSystemOperations
import System.Directory
import Data.ByteString (readFile, writeFile, empty, length)
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.Map.Strict as M ((!), (!?), size, delete, elems, Map, singleton, empty, fromList, union, insert, assocs, keys, member)
import Data.Time.Clock (UTCTime)
import Data.List (partition, foldl', elem)
import Control.Arrow ((***))
import Control.Monad (join, when, void)
import System.FilePath (splitDirectories, addTrailingPathSeparator, combine, takeExtension)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer.Strict
import Data.Maybe (fromJust)
import System.Directory.Internal (Permissions (..), writable)

--condition of shell
data CondShell = CondShell {
    currentPath :: Path
  , mapDirs :: Map Path Direct
  , root :: Path
  , currentDirect :: Direct
} deriving (Eq, Show)

getDirFromMap shell ind = mapDirs shell ! ind

saveDirectImpl :: CondShell -> Path -> IO()
saveDirectImpl shell path = do
  let curDir = getDirFromMap shell path
  let fullPath = makeFilePath path
  exist2 <- doesFileExist fullPath
  exist1 <- doesDirectoryExist fullPath
  if not exist1 && not exist2 then do
    createDirectoryIfMissing True fullPath
    mapM_ saveFileImpl (elems (filesInsideDir curDir))
    mapM_ (saveDirectImpl shell) (elems (dirsInsideDir curDir))
  else if exist1
    then do
      perm <- getPermissions fullPath
      if writable perm then do
         createDirectoryIfMissing True fullPath
         mapM_ saveFileImpl (elems (filesInsideDir curDir))
         mapM_ (saveDirectImpl shell) (elems (dirsInsideDir curDir))
      else putStrLn ("Error occured during writing directory " ++ fullPath ++ "because we haven`t access")
  else
     putStrLn ("Error occured during saving directory " ++ fullPath ++ "because this name is busy")

saveMain :: CondShell -> IO()
saveMain shell = do
     saveDirectImpl shell $ root shell
     delMaybe shell $ root shell

delMaybe :: CondShell -> Path -> IO ()
delMaybe shell path = do
  let curDir = getDirFromMap shell path
  let curPath = path
  let dirs = dirsInsideDir (fromJust (mapDirs shell !? curPath))
  let namesDirs = map ((:[]) . head) (elems dirs)
  let files = filesInsideDir (fromJust (mapDirs shell !? curPath))
  let namesFiles = map ((:[]) . nameFile) (elems files)
  realAll <- listDirectory (makeFilePath curPath)
  let realAllPaths = map (reverse . splitDirectories) realAll
  let needDel = filter (\x -> not(elem x namesDirs) && not(elem x namesFiles)) realAllPaths
  mapM_ (\name -> (removePathForcibly (makeFilePath (name ++ path)))) needDel
  mapM_ (delMaybe shell) (elems (dirsInsideDir curDir))


makeShell :: FilePath -> IO CondShell
makeShell filepath = do
    let path = (reverse . splitDirectories) filepath
    directs <- readDirectImpl path
    let cur = directs ! path
    --path map root dir
    return $ CondShell path directs path cur

type Action = ExceptT Errors (ReaderT UTCTime (WriterT ByteString (State CondShell)))

--executable, readable, searchable, writable
initPerm :: Permissions
initPerm = Permissions True True False False

updateTimeSize :: Path -> Action ()
updateTimeSize path = do
   time <- ask
   allMap <- gets mapDirs
   curPath <- gets currentPath
   case allMap !? path of
      Just direct -> do
        let newDirect = direct {modiDir = time}
        modify (\cur -> do
                  let modiMap = insert path newDirect allMap
                  cur {mapDirs = modiMap, currentDirect = modiMap ! curPath}
                 )
        updateTimeSize (pathDir newDirect)
      Nothing -> return ()

makeNewFileImpl :: Direct -> Path -> Name -> Action File
makeNewFileImpl parent path name = do
   let curPerms = permDir parent
   if writable curPerms then do
       time <- ask
       let oldParents = filesInsideDir parent
       allMap <- gets mapDirs
       let newFile = File name 0 initPerm path Data.ByteString.empty time
       let newParents = parent {filesInsideDir = insert name newFile oldParents}
       modify (\cur -> cur {mapDirs = insert path newParents allMap})
       updateTimeSize path
       return newFile
   else throwError $ ErrorMakeFile path "File isnt writable"

makeNewDirImpl :: Direct -> Path -> Name -> Action Direct
makeNewDirImpl parent path name = do
   let curPerms = permDir parent
   if writable curPerms then do
     time <- ask
     let oldParents = dirsInsideDir parent
     allMap <- gets mapDirs
     let path2 = concate path [name]
     let newParents = parent {dirsInsideDir = insert name path2 oldParents}
     let mDir = Direct name 0 initPerm path time M.empty M.empty
     modify (\cur -> do
              let news1 = insert path newParents allMap
              cur {mapDirs = insert path2 mDir news1}
            )
     updateTimeSize path
     return mDir
   else throwError $ ErrorMakeDir path "Folder isnt writable"

makeCdImpl :: Direct -> Path -> Action()
makeCdImpl curDir path = modify (\cur -> cur {currentPath = path, currentDirect = curDir})

makeCatImpl :: File -> Action()
makeCatImpl file = tell (contFile file)

makeWriteFileImlp :: ByteString -> File -> Path -> Action()
makeWriteFileImlp txt file path = do
    let curPerms = permFile file
    if writable curPerms then do
        time <- ask
        let len = toInteger (Data.ByteString.length txt)
        let parent = pathFile file
        let name = nameFile file
        allMap <- gets mapDirs
        let oldParent = allMap ! parent
        let newFile = file {contFile = txt, sizeFile = len, modiFile = time}
        let newParent = oldParent {filesInsideDir = insert name newFile (filesInsideDir oldParent)}
        modify (\cur -> cur {mapDirs = insert parent newParent allMap})
        updateTimeSize parent
    else
      throwError $ ErrorWrite path "File isnt writable"

makeContinueWriteFileImlp :: ByteString -> File -> Path -> Action()
makeContinueWriteFileImlp txt file path = do
    let curPerms = permFile file
    if writable curPerms then do
        time <- ask
        let len = toInteger (Data.ByteString.length txt)
        let parent = pathFile file
        let name = nameFile file
        allMap <- gets mapDirs
        let oldParent = allMap ! parent
        let newFile = file {contFile = pack(unpack (contFile file) <> unpack txt), sizeFile = len, modiFile = time}
        let newParent = oldParent {filesInsideDir = insert name newFile (filesInsideDir oldParent)}
        modify (\cur -> cur {mapDirs = insert parent newParent allMap})
        updateTimeSize parent
    else
      throwError $ ErrorWrite path "File isnt writable"

makeDelFileImpl :: File -> Path -> Action()
makeDelFileImpl file path = do
    let curPerms = permFile file
    if writable curPerms then do
      let parent = pathFile file
      let name = nameFile file
      allMap <- gets mapDirs
      let oldParent = allMap ! parent
      let newParent = oldParent {filesInsideDir = delete name (filesInsideDir oldParent)}
      modify (\cur -> do
                let news1 = insert parent newParent allMap
                cur {mapDirs = delete path news1}
             )
      updateTimeSize parent
    else
      throwError $ ErrorDelFile path "File isnt writable"

makeDelDirImpl :: Direct -> Path -> Action()
makeDelDirImpl direct path = do
    let curPerms = permDir direct
    let curSize = size (filesInsideDir direct) + size (dirsInsideDir direct)
    curWork <- gets currentPath
    if curWork == path then
      throwError $ ErrorDelDir path "Direct is current working place"
    else
        if writable curPerms && (curSize == 0) then do
          let parent = pathDir direct
          let name = nameDir direct
          allMap <- gets mapDirs
          let oldParent = allMap ! parent
          let newParent = oldParent {dirsInsideDir = delete name (dirsInsideDir oldParent)}
          modify (\cur -> do
                    let news1 = insert parent newParent allMap
                    cur {mapDirs = delete path news1}
                 )
          updateTimeSize parent
        else if curSize == 0 then
          throwError $ ErrorDelDir path "Direct isnt writable"
        else
          throwError $ ErrorDelDir path "Direct isnt empty"

directInfoImpl :: Direct -> Path -> Action ()
directInfoImpl direct path = do
   tell "FILES INSIDE: "
   tell (pack (show (size (filesInsideDir direct))))
   tell "\n"
   tell "DIRECTORIES INSIDE: "
   tell (pack (show (size (dirsInsideDir direct))))
   tell "\n"
   tell "MODIFICATION TIME: "
   tell (pack (show (modiDir direct)))
   tell "\n"
   tell "SIZE: "
   tell (pack (show (sizeDir direct)))
   tell "\n"
   tell "PATH: "
   tell (pack (makeFilePath(pathDir direct)))
   tell "\n"
   tell "PERMISSIONS: "
   let perms = permDir direct
   outPerms perms

outPerms perms = case perms of
                       (Permissions r w e s) -> do
                          if r then tell "Readable " else tell ""
                          if w then tell "Writable " else tell ""
                          if e then tell "Executable " else tell ""
                          if s then tell "Searchable " else tell ""

fileInfoImpl :: File -> Path -> Action ()
fileInfoImpl file path = do
   tell "MODIFICATION TIME: "
   tell (pack (show (modiFile file)))
   tell "\n"
   tell "SIZE: "
   tell (pack (show (sizeFile file)))
   tell "\n"
   tell "PATH: "
   tell (pack (makeFilePath(pathFile file)))
   tell "\n"
   tell "PERMISSIONS: "
   let perms = permFile file
   outPerms perms
   tell "\n"
   tell "EXTENSION: "
   let extension = takeExtension (nameFile file)
   if extension == "" then
     tell (pack "unknown")
   else
     tell (pack extension)


makeFindFileImpl :: Name -> FilePath -> Direct -> Action ()
makeFindFileImpl name pref curDir = do
   mapM_ (helperFind pref name) (keys (filesInsideDir curDir))
   allMap <- gets mapDirs
   mapM_ (\(nowName, nowDirectPath) -> helperFind pref name nowName >>
             makeFindFileImpl name (combine pref nowName)(allMap ! nowDirectPath)) (assocs (dirsInsideDir curDir))
   
helperFind :: FilePath -> Name -> Name -> Action()
helperFind pref2 name name2  =
   when (name == name2) (tell (pack (combine pref2 name2) <> "\n"))

makeAction :: FilePath -> (Direct -> Path -> Action()) -> (File -> Path -> Action()) ->
              (Direct -> Path -> String -> Action()) -> Action()
makeAction filepath caseDir caseFile caseFree = do
     cur <- get
     let curPath = currentPath cur
     let splittedPath = (reverse . splitDirectories) filepath
     let absPath = concate curPath splittedPath
     let name = head absPath
     let parentPath = tail absPath
     case mapDirs cur !? absPath of
        Just direct -> caseDir direct absPath
        Nothing ->
           case mapDirs cur !? parentPath of
               Just curDir -> case filesInsideDir curDir !? name of
                    Just f -> caseFile f absPath
                    Nothing -> caseFree curDir parentPath name
               Nothing -> throwError $ ErrorNoDir parentPath "No such directory"


makeCommand :: Command -> Action ()
makeCommand (MakeFile path) = makeAction path (\x y -> throwError $ ErrorMakeFile y "Direct with this name already exist")
                               (\x y -> throwError $ ErrorMakeFile y "File with this name already exist") (((void .) .) . makeNewFileImpl)
makeCommand (MakeDir path) = makeAction path (\x y -> throwError $ ErrorMakeDir y "Direct with this name already exist")
                               (\x y -> throwError $ ErrorMakeDir y "File with this name already exist") (((void .) .) . makeNewDirImpl)
makeCommand (Cd path) = makeAction path makeCdImpl (\x y -> throwError $ ErrorNoDir y "have the other type (not directory)")
                               (\x y str -> throwError $ ErrorNoDir (concate y [str]) "No such direct")
makeCommand LsCur = do
   cur <- get
   let curPath = currentPath cur
   let dirs = dirsInsideDir (fromJust (mapDirs cur !? curPath))
   let files = filesInsideDir (fromJust (mapDirs cur !? curPath))
   tell (pack "DIRECTORIES:\n")
   mapM_ (\curKey -> tell (pack curKey <> "\n")) (keys dirs)
   tell (pack "FILES:\n")
   mapM_ (\curKey -> tell (pack curKey <> "\n")) (keys files)
   
makeCommand CurLoc = do
   cur <- get
   let curPath = currentPath cur
   tell (pack "YOU ARE CURRENTLY HERE: ")
   tell (pack (makeFilePath curPath))
   
makeCommand (Ls path) = do
   cur <- get
   let curPath = currentPath cur
   let splittedPath = (reverse . splitDirectories) path
   let absPath = concate curPath splittedPath
   case mapDirs cur !? absPath of
           Just direct -> do
               let dirs = dirsInsideDir (fromJust (mapDirs cur !? absPath))
               let files = filesInsideDir (fromJust (mapDirs cur !? absPath))
               tell (pack "DIRECTORIES:\n")
               mapM_ (\curKey -> tell (pack curKey <> "\n")) (keys dirs)
               tell (pack "FILES:\n")
               mapM_ (\curKey -> tell (pack curKey <> "\n")) (keys files)
           Nothing -> throwError $ ErrorNoDir absPath "No such directory"

makeCommand (Cat path) = makeAction path (\x y -> throwError $ ErrorNoFile y "have the other type (not file)") (flip $ const makeCatImpl)
                               (\x y str -> throwError $ ErrorNoFile (concate y [str]) "No such file")
makeCommand (DelFile path) = makeAction path (\x y -> throwError $ ErrorDelFile y "have the other type (not file)") makeDelFileImpl
                               (\x y str -> throwError $ ErrorDelFile (concate y [str]) "No such file")
makeCommand (DelDir path) = makeAction path makeDelDirImpl (\x y -> throwError $ ErrorDelDir y "have the other type (not directory)")
                               (\x y str -> throwError $ ErrorDelDir (concate y [str]) "No such direct")
makeCommand (WriteFile path txt) = makeAction path (\x y -> throwError $ ErrorWrite y "have the other type (not file)") (makeWriteFileImlp txt)
                               (\x y str -> throwError $ ErrorWrite (concate y [str]) "No such file")
makeCommand (ContWriteFile path txt) = makeAction path (\x y -> throwError $ ErrorWrite y "have the other type (not file)") (makeContinueWriteFileImlp txt)
                               (\x y str -> throwError $ ErrorWrite (concate y [str]) "No such file")
makeCommand (FindFile path name) = makeAction path (\x y -> makeFindFileImpl name (addTrailingPathSeparator path) x) (\x y -> throwError $ ErrorFind y "have the other type (not directory)")
                               (\x y str -> throwError $ ErrorNoDir (concate y [str]) "No such direct")
makeCommand (Inform path) = makeAction path directInfoImpl fileInfoImpl
                               (\x y str -> throwError $ ErrorNoFile (concate y [str]) "No such file")
makeCommand other = return ()