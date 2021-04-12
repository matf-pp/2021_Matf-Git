module Command.Show (commandShow) where

import Command.Basic
import Options.Applicative

optInfo :: Parser PrintType
optInfo = flag' UserInfo (
             long "info"
             <> help "Prints user information"
             )

showOptions :: Parser Command
showOptions = Print <$> optDir <*> (optInfo)

commandShow :: Mod CommandFields Command
commandShow = command "show" (info showOptions (progDesc "Prints requested information"))