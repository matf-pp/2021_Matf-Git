module Command.Basic (
    SetType(SetUserName, SetEmail, AddIgnore, RemoveIgnore), 
    PrintType(PrintUserInfo, PrintIgnore), 
    Command(Greet, Init, Set, Print),
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
             | Init {directory :: String, force :: Bool}
             | Set  {directory :: String, setArgs :: [SetType]}
             | Print {directory :: String, printArg :: PrintType}
  deriving (Eq, Show)

optDir :: Parser String
optDir = strOption (long "directory"
                   <> short 'd' 
                   <> value ""
                   <> metavar "PATH" 
                   <> help "Path to directory to be initialised into a repository (default: current working directory)")

