module Command.Basic (
    SetType(SetUserName, SetEmail, AddIgnore, RemoveIgnore, AddRef, AddTag), 
    PrintType(PrintUserInfo, PrintIgnore, PrintHead),
    VisitType(VisitRef, VisitTag, VisitSha),
    Command(Init, Set, Print, Commit, Visit, GarbageCollector),
    optDir, directory
) where

import Options.Applicative

data SetType = SetUserName String
             | SetEmail String
             | AddIgnore FilePath
             | RemoveIgnore FilePath
             | AddRef String
             | AddTag String
  deriving (Eq, Show)
  
data PrintType = PrintUserInfo
               | PrintIgnore
               | PrintHead
  deriving (Eq, Show)

data VisitType = VisitRef {name :: String}
               | VisitTag {name :: String}
               | VisitSha {prefix :: String}
  deriving (Eq, Show)

data Command = Init {directory :: FilePath, force :: Bool}
             | Set  {directory :: FilePath, setArgs :: [SetType]}
             | Print {directory :: FilePath, printArg :: PrintType}
             | Commit {directory :: FilePath, message :: String}
             | Visit {directory :: FilePath, visitType :: VisitType}
             | GarbageCollector {directory :: FilePath}
  deriving (Eq, Show)

optDir :: Parser String
optDir = strOption (long "directory"
                   <> short 'd' 
                   <> value ""
                   <> metavar "PATH" 
                   <> help "Path to directory to be initialised into a repository (default: current working directory)")

