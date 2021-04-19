module Command.Set (commandSet) where

import Command.Basic
import Options.Applicative

optUserName :: Parser SetType
optUserName = SetUserName <$> strOption (
                 long "username"
                 <> short 'u'
                 <> metavar "USER_NAME"
                 <> help "Pass the user name of the user writing to the repository"
                 )

optEmail :: Parser SetType
optEmail = SetEmail <$> strOption (
              long "email"
              <> metavar "EMAIL"
              <> help "Pass the email of the user writing to the repository"
              )

optAddIgnore :: Parser SetType
optAddIgnore = AddIgnore <$> strOption (
              long "add-ignore"
              <> metavar "PATH"
              <> help "Pass the filepath to be added to the list of ignores"
              )

optRemoveIgnore :: Parser SetType
optRemoveIgnore = RemoveIgnore <$> strOption (
              long "remove-ignore"
              <> metavar "PATH"
              <> help "Pass the filepath to be removed to the list of ignores"
              )

setOptions :: Parser Command
setOptions = Set <$> optDir <*> some (optEmail <|> optUserName <|> optAddIgnore <|> optRemoveIgnore)

commandSet :: Mod CommandFields Command
commandSet = command "set" (info setOptions (progDesc "Sets repository specific information"))