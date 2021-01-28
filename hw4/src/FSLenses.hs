{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module FSLenses where

import Lens.Micro
import Data.List (partition)
import System.FilePath
import Control.Monad
import System.Directory
import Control.Arrow ((***))

data FS
    = Dir
    { name     :: FilePath  -- just name of dir
    , contents :: [FS]
    }
    | File
    { name     :: FilePath  -- just name of file
    } deriving (Eq, Show)

-- HERE 6TH and 7TH TASKS

-- | Traversable lens for name of Dir
nameDir :: Traversal' FS FilePath
nameDir fun struc = case struc of
                       f@(File curName) -> pure f
                       d@(Dir curName curContents) -> fmap Dir (fun curName) <*> pure curContents


-- | Traversable lens for name of File
nameFile :: Traversal' FS FilePath
nameFile fun struc = case struc of
                       f@(File curName) -> fmap File (fun curName)
                       d@(Dir curName curContents) -> pure d


-- | Traversable lens for content of Dir
getContents :: Traversal' FS FS
getContents fun struc = case struc of
                       f@(File curName) -> pure f
                       d@(Dir curName curContents) -> fmap (Dir curName) (traverse fun curContents)

-- | Lens for name (will help in ls)
allName :: Lens' FS FilePath
allName = lens name (\file curName -> file { name = curName })

------------------------------------------
-- | NECESSARY function from 6TH TASK | --
------------------------------------------

-- | Create FS with filepath of real existing file system
getDirectory :: FilePath -> IO FS
getDirectory filepath = do
        let listDir = listDirectory filepath -- IO [FilePath]
        let checker = doesDirectoryExist
        cont <- map (filepath </>) <$> listDir --names of all content of cur directory
        typeCont <- mapM checker cont
        let zipped = zip cont typeCont
        let participate = partition snd zipped --make two lists - in first files, second is dirs
        let (dirsNames, filesNames) = join (***) (map fst) participate
        dirs <- mapM getDirectory dirsNames
        let fileNames2 = map takeFileName filesNames
        let files = map File fileNames2
        return (Dir (takeFileName filepath) (files ++ dirs))

-------------------------------------------------------------------------------
-- | UNNECESSARY but helpful functions from 6TH task (I honestly think so) | --
-------------------------------------------------------------------------------

-- | 1: subtrees for Dir or emply list
-- | check if we have Dir instance or File and filter by this condition
firstFunc :: FS -> [FS]
firstFunc mayDir = mayDir ^.. FSLenses.getContents . filtered (\case
                                                           (File _) -> False
                                                           (Dir _ _) -> True
                                                                )

-- | 2: maybe with name of Dir
secondFunc :: FS -> Maybe FilePath
secondFunc mayDir = case mayDir of
                      (File _) -> Nothing
                      (Dir _ _) -> Just (head (mayDir ^.. nameDir))

-- | 3: name of File or empty string
thirdFunc :: FS -> FilePath
thirdFunc mayFile = mayFile ^. nameFile

-- | 4: change name of root with /
-- | & is a reverse application operator. This provides notational convenience. (c)Hackage
fourthFunc :: FS -> FS
fourthFunc root = root & nameDir .~ "/"

-- | 5: add suffix to root's name
fifthFunc :: String -> FS -> FS
fifthFunc suffix root = do
     let cur = root ^. nameDir
     let newSt = cur ++ suffix
     root & nameDir .~ newSt

-- | 6: name of first subdir or Nothing
-- | ^? - returns Nothing if there`s no such value
sixthFunc :: FS -> Maybe FilePath
sixthFunc cur = cur ^? (FSLenses.getContents . nameDir)

-- | 7: name of Files in Dir iteratively
-- | Almost exactly like 6th function but we're sure about List
seventhFunc :: FS -> [FilePath]
seventhFunc cur = cur ^.. (FSLenses.getContents . nameFile)

------------------------------------------
-- | NECESSARY function from 7TH TASK | --
------------------------------------------

-- | cd: go to the given subdir
cd :: FilePath -> Traversal' FS FS
cd subDir = FSLenses.getContents . filtered (\case
                                                (File _) -> False
                                                (Dir curName _) -> curName == subDir
                                              )

-- | ls: list of names of every content in dir
ls :: Traversal' FS FilePath
ls = FSLenses.getContents . allName

-- | file: take name of the given File if exists
-- | failing - if first rule fails we activate the second one
-- | find the one with such name by filter and get it's name
file :: FilePath -> Traversal' FS FilePath
file name = failing (nameFile . filtered (==name))
                 (FSLenses.getContents . filtered (\x ->
                                                      (x ^. nameFile) == name) . nameFile)
