module Command (
    --Types
    SetType(SetUserName, SetEmail, AddIgnore, RemoveIgnore), 
    PrintType(PrintUserInfo, PrintIgnore), 
    Command(Greet, Init, Set, Print),

    --Other
    execOpt
) where

import Command.Basic
import Command.Init
import Command.Set
import Command.Show

import Options.Applicative

optProgram :: Parser Command
optProgram = hsubparser (
              command "greet" (info (pure Greet) (progDesc "Print greeting"))
           <> commandInit
           <> commandSet
           <> commandShow
         )

opts :: ParserInfo Command
opts = info (optProgram <**> helper) (fullDesc
        <> header "LeGit - a SVC implemented in Haskell (somewhat based on git)" )

execOpt :: IO Command
execOpt = execParser opts
