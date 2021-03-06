module Command.Merge (commandMerge) where

import Command.Basic

import Options.Applicative

optBranch :: Parser String
optBranch = strOption ( long "branch"
                      <> short 'b'
                      <> metavar "BRANCH_NAME"
                      <> help "HEAD is merged into provided branch, then the branch is visited"
                       )

optMerge :: Parser Command 
optMerge = Merge <$> optDir <*> optBranch <*> optMessage 

commandMerge :: Mod CommandFields Command
commandMerge = command "merge" (info optMerge (progDesc "Merges two branches together"))