module Command (
    --Types
    SetType(SetUserName, SetEmail, AddIgnore, RemoveIgnore, AddRef, AddTag, RemoveRef, RemoveTag), 
    PrintType(PrintUserInfo, PrintIgnore, PrintHead, PrintStatus), 
    VisitType(VisitRef, VisitTag, VisitSha),
    Command(Init, Set, Print, Commit, Merge, Visit, GarbageCollector),
    directory,
    --Other
    execOpt
) where

import Command.Basic
import Command.Init
import Command.Set
import Command.Show
import Command.Commit
import Command.Visit
import Command.Merge
import Command.GarbageCollector

import Options.Applicative

optProgram :: Parser Command
optProgram = hsubparser (
              commandInit
           <> commandSet
           <> commandShow
           <> commandCommit
           <> commandMerge
           <> commandVisit
           <> commandGC
         )

opts :: ParserInfo Command
opts = info (optProgram <**> helper) (fullDesc
        <> header "LeGit - a SVC implemented in Haskell (somewhat based on git)" )

execOpt :: IO Command
execOpt = customExecParser (prefs showHelpOnEmpty) opts
