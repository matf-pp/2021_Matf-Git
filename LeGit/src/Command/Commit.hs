module Command.Commit (commandCommit) where

import Command.Basic

import Options.Applicative

optCommit :: Parser Command 
optCommit = Commit <$> optDir <*> optMessage

commandCommit :: Mod CommandFields Command
commandCommit = command "commit" (info optCommit (progDesc "Commits the current changes"))