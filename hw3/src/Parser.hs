module Parser where

import FileSystemTypes
import FileSystemOperations
import Shell
import Options.Applicative

--read https://hackage.haskell.org/package/optparse-applicative-0.16.1.0/docs/Options-Applicative-Extra.html
--https://hackage.haskell.org/package/optparse-applicative

parseMain :: String -> ParserResult Command
parseMain txt = execParserPure defaultPrefs (info (helper <*> parserCommandHSU)
                                               (fullDesc <> header "Shell" <> progDesc "This is shell"))
                                            (words txt)

parseCommand :: String -> Parser Command -> String -> Mod CommandFields Command
parseCommand name parser description = command name (info parser (progDesc description))

parserCommandHSU :: Parser Command
parserCommandHSU = hsubparser (makeCurLocCom <> makeNewFileCom <> makeNewDirCom <> makeCdCom <>
                              makeCatCom <> makeDelFileCom <> makeDelDirCom <>
                              makeWriteFileCom <> makeContinueWriteFileCom <> makeFindFileCom <>
                              makeLsCurCom <> makeLsCom <> makeInformCom <> saveCom <> exitCom)

makeNewFileCom :: Mod CommandFields Command
makeNewFileCom = parseCommand "makeFile"
                           (MakeFile <$> strArgument (metavar "*path of file*" <> help "path to place where create file"))
                           "Make new file. Usage: *name of new file*"

makeNewDirCom :: Mod CommandFields Command
makeNewDirCom = parseCommand "makeDir"
                           (MakeDir <$> strArgument (metavar "*path of dir*" <> help "path to place where create folder"))
                           "Make new folder. Usage: *name of new folder*"

makeCdCom :: Mod CommandFields Command
makeCdCom = parseCommand "cd"
                           (Cd <$> strArgument (metavar "*pathf of dir*" <> help "path to place where need to go"))
                           "Go to directory. Usage: *directory path*"

makeLsCurCom :: Mod CommandFields Command
makeLsCurCom = parseCommand "dir"
                           (pure LsCur)
                           "Print all files and directories inside current directory"

makeCurLocCom :: Mod CommandFields Command
makeCurLocCom = parseCommand "curLoc"
                           (pure CurLoc)
                           "Print your current location"

makeLsCom :: Mod CommandFields Command
makeLsCom = parseCommand "ls"
                           (Ls <$> strArgument (metavar "*pathf of dir*" <> help "path of dir to display"))
                           "Print all files and directories inside choosen directory. Usage: *path of directory*"

makeCatCom :: Mod CommandFields Command
makeCatCom = parseCommand "cat"
                           (Cat <$> strArgument (metavar "*path of file*" <> help "path to place where file contained"))
                           "Print file. Usage: *path of file*"

makeInformCom :: Mod CommandFields Command
makeInformCom = parseCommand "information"
                           (Inform <$> strArgument (metavar "*path of file*" <> help "path to place where file contained"))
                           "Print information about file or directory. Usage: *path of file or directory*"

makeDelFileCom :: Mod CommandFields Command
makeDelFileCom = parseCommand "delFile"
                           (DelFile <$> strArgument (metavar "*path of file*" <> help "path to place where delete file"))
                           "Delete the file. Usage: *path of file*"

makeDelDirCom :: Mod CommandFields Command
makeDelDirCom = parseCommand "delDir"
                           (DelDir <$> strArgument (metavar "*path of dir*" <> help "path to place where delete folder"))
                           "Delete the folder. Usage: *path of directory*"

makeWriteFileCom :: Mod CommandFields Command
makeWriteFileCom = parseCommand "writeFile"
                           (WriteFile <$> strArgument (metavar "*pa th of file*" <> help "path to place where file contained") <*>
                           strArgument (metavar "*text of content*" <> help "text which need to write to file")
                           )
                           "Write text to the file. Usage: *path of file*, *text to write*"

makeContinueWriteFileCom :: Mod CommandFields Command
makeContinueWriteFileCom = parseCommand "contWriteFile"
                           (ContWriteFile <$> strArgument (metavar "*path of file*" <> help "path to place where file contained") <*>
                           strArgument (metavar "*text of content*" <> help "text which need to add to file")
                           )
                           "Add text to the existing text in file. Usage: *path of file*, *text to write*"

makeFindFileCom :: Mod CommandFields Command
makeFindFileCom = parseCommand "findFile"
                           (FindFile <$> strArgument (value "" <> metavar "*path of dir*" <> help "path to place where to start searching file") <*>
                           strArgument (value "" <> metavar "*name of file*" <> help "name of file to find")
                           )
                           "Find file by his name in directory or sub-directories. Usage: *path of directrion where to start searching*, *name to find*"

saveCom :: Mod CommandFields Command
saveCom = parseCommand "save"
                          (pure Save)
                          "Save changes"

exitCom :: Mod CommandFields Command
exitCom = parseCommand "exit"
                          (pure Exit)
                          "Save changes and exit"