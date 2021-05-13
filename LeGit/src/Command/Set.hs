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
              <> short 'e'
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

optAddRef :: Parser SetType
optAddRef = AddRef <$> strOption (
            long "new-branch"
            <> short 'b'
            <> metavar "NEW_NAME"
            <> help "Creates a new branch from the current head"
            )

optAddTag :: Parser SetType
optAddTag = AddTag <$> strOption (
            long "new-tag"
            <> short 't'
            <> metavar "NEW_NAME"
            <> help "Stores the current head as a tagged special commit"
            )

optRemoveRef :: Parser SetType
optRemoveRef = RemoveRef <$> strOption (
            long "remove-branch"
            <> metavar "NAME"
            <> help "Deletes a branch (does not delete it's commits)"
            )

optRemoveTag :: Parser SetType
optRemoveTag = RemoveTag <$> strOption (
            long "remove-tag"
            <> metavar "TAG"
            <> help "Deletes a tag (does not delete it's commits)"
            )

setOptions :: Parser Command
setOptions = Set <$> optDir <*> some (    optEmail 
                                      <|> optUserName 
                                      <|> optAddIgnore 
                                      <|> optRemoveIgnore 
                                      <|> optAddRef 
                                      <|> optAddTag
                                      <|> optRemoveRef
                                      <|> optRemoveTag
                                        )

commandSet :: Mod CommandFields Command
commandSet = command "set" (info setOptions (progDesc "Sets repository specific information"))