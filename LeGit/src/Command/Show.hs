module Command.Show (commandShow) where

import Command.Basic
import Options.Applicative

optInfo :: Parser PrintType
optInfo = flag' PrintUserInfo (
             long "info"
             <> help "Prints user information"
             )

optStatus :: Parser PrintType
optStatus = flag' PrintStatus (
             long "status"
             <> short 's'
             <> help "Prints changes made compared to HEAD"
             )

optIgnore :: Parser PrintType
optIgnore = flag' PrintIgnore (
             long "ignore"
             <> help "Prints ignored files and directories"
             )

optHead :: Parser PrintType
optHead = flag' PrintHead (
             long "head"
             <> help "Prints the current value of HEAD"
             )

optRefs :: Parser PrintType
optRefs = flag' PrintRefs (
             long "branches"
             <> short 'b'
             <> help "Prints existing branches"
             )

optTags :: Parser PrintType
optTags = flag' PrintTags (
             long "tags"
             <> short 't'
             <> help "Prints existing tags"
             )

showOptions :: Parser Command
showOptions = Print <$> optDir <*> (optInfo 
                                <|> optIgnore 
                                <|> optHead 
                                <|> optStatus
                                <|> optRefs
                                <|> optTags
                                )

commandShow :: Mod CommandFields Command
commandShow = command "show" (info showOptions (progDesc "Prints requested information"))