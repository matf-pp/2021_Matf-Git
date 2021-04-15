module Command.Show (commandShow) where

import Command.Basic
import Options.Applicative

optInfo :: Parser PrintType
optInfo = flag' PrintUserInfo (
             long "info"
             <> help "Prints user information"
             )

optIgnore :: Parser PrintType
optIgnore = flag' PrintIgnore (
             long "ignore"
             <> help "Prints ignored files and directories"
             )

showOptions :: Parser Command
showOptions = Print <$> optDir <*> (optInfo <|> optIgnore)

commandShow :: Mod CommandFields Command
commandShow = command "show" (info showOptions (progDesc "Prints requested information"))