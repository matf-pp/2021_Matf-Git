module Command.Basic (
    SetType(SetUserName, SetEmail, AddIgnore, RemoveIgnore, AddRef, AddTag, RemoveRef, RemoveTag), 
    PrintType(PrintUserInfo, PrintIgnore, PrintHead, PrintStatus),
    VisitType(VisitRef, VisitTag, VisitSha),
    Command(Init, Set, Print, Commit, Merge, Visit, GarbageCollector),
    optDir, directory
) where

import Options.Applicative

data SetType = SetUserName String
             | SetEmail String
             | AddIgnore FilePath
             | RemoveIgnore FilePath
             | AddRef String
             | AddTag String
             | RemoveRef String
             | RemoveTag String
  deriving (Eq, Show)
  
data PrintType = PrintUserInfo
               | PrintIgnore
               | PrintHead
               | PrintStatus
  deriving (Eq, Show)

data VisitType = VisitRef {name :: String}
               | VisitTag {name :: String}
               | VisitSha {prefix :: String}
  deriving (Eq, Show)

data Command = Init {directory :: FilePath, force :: Bool}
             | Set  {directory :: FilePath, setArgs :: [SetType]}
             | Print {directory :: FilePath, printArg :: PrintType}
             | Commit {directory :: FilePath, message :: String}
             | Merge {directory :: FilePath, branch :: String, message :: String}
             | Visit {directory :: FilePath, visitType :: VisitType}
             | GarbageCollector {directory :: FilePath}
  deriving (Eq, Show)

optDir :: Parser String
optDir = strOption (long "directory"
                   <> short 'd' 
                   <> value ""
                   <> metavar "PATH" 
                   <> help "Path to directory to be initialised into a repository (default: current working directory)")

