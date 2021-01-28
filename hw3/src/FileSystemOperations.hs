{-# LANGUAGE OverloadedStrings     #-}

module FileSystemOperations where

import FileSystemTypes
import System.Directory
import Data.ByteString (readFile, writeFile)
import Data.Map.Strict as M (Map, singleton, empty, fromList, union, insert)
import Data.Time.Clock (UTCTime)
import Data.List (partition, foldl')
import Control.Arrow ((***))
import Control.Monad (join, when)

getProps :: FilePath -> IO (UTCTime, Integer, Permissions)
getProps filepath = do
    modi <- getModificationTime filepath
    size <- getFileSize filepath
    perm <- getPermissions filepath
    return (modi, size, perm)

readFileImpl :: Path -> IO File
readFileImpl path = do
    let filepath = makeFilePath path
    (modi, size, perm) <- getProps filepath
    --check permissions
    cont <- if readable perm then
              Data.ByteString.readFile filepath else
                return ""
     --name size perm path cont modi
    return $ File (head path) size perm (tail path) cont modi

saveFileImpl :: File -> IO()
saveFileImpl file = do
    let fullPath = makeFilePath $ concate (pathFile file) [nameFile file]
    exist1 <- doesFileExist fullPath
    exist2 <- doesDirectoryExist fullPath
    if not exist1 && not exist2 then do
      Data.ByteString.writeFile fullPath (contFile file)
      setModificationTime fullPath (modiFile file)
      setPermissions fullPath (permFile file)
    else if exist1 
      then do
        perm <- getPermissions fullPath
        if writable perm && not (executable perm)then do
           Data.ByteString.writeFile fullPath (contFile file)
           setModificationTime fullPath (modiFile file)
           setPermissions fullPath (permFile file)
        else putStr "" --putStrLn ("Error occured during writing to file " ++ fullPath ++ "because we haven`t access")
    else
       putStrLn ("Error occured during saving file " ++ fullPath ++ "because this name is busy")

readDirectImpl :: Path -> IO (Map Path Direct)
readDirectImpl path = do
    let filepath = makeFilePath path
    (modi, size, perm) <- getProps filepath
    --name size perm path modi filesInside dirsInside
    if readable perm
      then do
        let listDir = listDirectory filepath -- IO [FilePath]
        let checker = doesDirectoryExist . makeFilePath
        cont <- map (\curDir -> curDir : path) <$> listDir --names of all content of cur directory
        typeCont <- mapM checker cont
        let zipped = zip cont typeCont
        let participate = partition snd zipped --make two lists - in first files, second is dirs
        let (dirsNames, filesNames) = join (***) (map fst) participate
        dirs <- mapM readDirectImpl dirsNames
        files <- mapM readFileImpl filesNames
        let filesInside = map (\curFile -> (nameFile curFile, curFile)) files
        let dirsInside = map (\curDir -> (head curDir, curDir)) dirsNames
        let newDir = Direct (head path) size perm (tail path) modi (M.fromList filesInside) (M.fromList dirsInside)
        let theMap = foldl' M.union M.empty dirs
        return $ M.insert path newDir theMap
      else do
        let newDir = Direct (head path) size perm (tail path) modi M.empty M.empty
        return $ M.singleton path newDir
        
