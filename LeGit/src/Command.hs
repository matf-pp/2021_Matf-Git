module Command (
    --Types
    SetType(SetUserName, SetEmail, AddIgnore, RemoveIgnore, AddRef, AddTag), 
    PrintType(PrintUserInfo, PrintIgnore), 
    VisitType(VisitRef, VisitTag, VisitSha),
    Command(Greet, Init, Set, Print, Commit, Visit),

    --Other
    execOpt
) where

import Command.Basic
import Command.Init
import Command.Set
import Command.Show
import Command.Commit
import Command.Visit

import Options.Applicative

optProgram :: Parser Command
optProgram = hsubparser (
              command "greet" (info (pure Greet) (progDesc "Print greeting"))
           <> commandInit
           <> commandSet
           <> commandShow
           <> commandCommit
           <> commandVisit
         )

opts :: ParserInfo Command
opts = info (optProgram <**> helper) (fullDesc
        <> header "LeGit - a SVC implemented in Haskell (somewhat based on git)" )

execOpt :: IO Command
execOpt = customExecParser (prefs showHelpOnEmpty) opts
