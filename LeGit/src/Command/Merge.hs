module Command.Merge (commandMerge) where

import Command.Basic

import Options.Applicative

optMessage :: Parser String
optMessage = strOption ( long "message"
                      <> short 'm'
                      <> value "No message added"
                      <> metavar "MESSAGE"
                      <> help "Message to be appended to the commit"
                       )

optBranch :: Parser String
optBranch = strOption ( long "branch"
                      <> short 'b'
                      <> metavar "BRANCH_NAME"
                      <> help "Branch with witch HEAD is merged"
                       )

optMerge :: Parser Command 
optMerge = Merge <$> optDir <*> optBranch <*> optMessage 

commandMerge :: Mod CommandFields Command
commandMerge = command "merge" (info optMerge (progDesc "Merges two branches together"))