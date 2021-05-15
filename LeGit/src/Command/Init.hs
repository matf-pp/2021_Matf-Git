module Command.Init (commandInit) where

import Command.Basic
import Options.Applicative


optForce :: Parser Bool
optForce = switch (long "force"
                  <> short 'f'
                  <> help "Forces initialization (may overwrite previous repo info and delete files named after required repo info files)")


initOptions :: Parser Command
initOptions = Init <$> optDir <*> optForce

commandInit :: Mod CommandFields Command
commandInit = command "init" (info initOptions (progDesc "Initializes a directory into a LeGit repo"))