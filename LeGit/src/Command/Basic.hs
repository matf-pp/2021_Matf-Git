module Command.Basic (
    SetType(SetUserName, SetEmail, AddIgnore, RemoveIgnore), 
    PrintType(PrintUserInfo, PrintIgnore),
    Command(Greet, Init, Set, Print, Commit),
    optDir
) where

import Options.Applicative

data SetType = SetUserName String
             | SetEmail String
             | AddIgnore FilePath
             | RemoveIgnore FilePath
  deriving (Eq, Show)
  
data PrintType = PrintUserInfo
               | PrintIgnore
  deriving (Eq, Show)

data Command = Greet
             | Init {directory :: FilePath, force :: Bool}
             | Set  {directory :: FilePath, setArgs :: [SetType]}
             | Print {directory :: FilePath, printArg :: PrintType}
             | Commit {directory :: FilePath, message :: String}
  deriving (Eq, Show)

optDir :: Parser String
optDir = strOption (long "directory"
                   <> short 'd' 
                   <> value ""
                   <> metavar "PATH" 
                   <> help "Path to directory to be initialised into a repository (default: current working directory)")

