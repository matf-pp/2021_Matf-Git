module Command.Revert (commandRevert) where

import Command.Basic

import Options.Applicative

optNum :: Parser Int
optNum = read <$> strOption ( long "num"
                      <> short 'n'
                      <> value "1"
                      <> metavar "NUMBER"
                      <> help "Number of commits to revert"
                       )

optRevert :: Parser Command 
optRevert = Revert <$> optDir <*> optNum <*> optMessage 

commandRevert :: Mod CommandFields Command
commandRevert = command "revert" (info optRevert (progDesc "Restore branch to a previous version by making new commits"))