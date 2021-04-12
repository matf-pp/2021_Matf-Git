module Command.Basic (
    SetType(UserName, Email), 
    PrintType(UserInfo), 
    Command(Greet, Init, Set, Print),
    optDir
) where

import Options.Applicative

data SetType = UserName String
             | Email String
  deriving (Eq, Show)
  
data PrintType = UserInfo
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

