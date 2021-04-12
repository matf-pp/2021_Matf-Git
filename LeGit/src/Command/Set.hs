module Command.Set (commandSet) where

import Command.Basic
import Options.Applicative

optUserName :: Parser SetType
optUserName = UserName <$> strOption (
                 long "username"
                 <> short 'u'
                 <> metavar "USER_NAME"
                 <> help "Pass the user name of the user writing to the repository"
                 )

optEmail :: Parser SetType
optEmail = Email <$> strOption (
              long "email"
              <> metavar "EMAIL"
              <> help "Pass the email of the user writing to the repository"
              )

setOptions :: Parser Command
setOptions = Set <$> optDir <*> some (optEmail <|> optUserName)

commandSet :: Mod CommandFields Command
commandSet = command "set" (info setOptions (progDesc "Sets repository specific information"))