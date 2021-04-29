module Command.Commit (commandCommit) where

import Command.Basic

import Options.Applicative

optMessage :: Parser String
optMessage = strOption ( long "message"
                      <> short 'm'
                      <> value "No message added"
                      <> metavar "MESSAGE"
                      <> help "Message to be appended to the commit"
                       )

optCommit :: Parser Command 
optCommit = Commit <$> optDir <*> optMessage

commandCommit :: Mod CommandFields Command
commandCommit = command "commit" (info optCommit (progDesc "Commits the current changes"))