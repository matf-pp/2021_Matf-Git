module Command.Visit (commandVisit) where

import Command.Basic
import Options.Applicative


optRef :: Parser VisitType
optRef = VisitRef <$> strOption(
        long "branch"
     <> short 'b'
     <> metavar "BRANCH_NAME"
       )

optTag :: Parser VisitType
optTag = VisitTag <$> strOption(
        long "tag"
     <> short 't'
     <> metavar "TAG_NAME"
       )

optSha :: Parser VisitType
optSha = VisitSha <$> strOption(
        long "sha"
     <> short 's'
     <> metavar "COMMIT_SHA_PREFIX"
       )

optVisit :: Parser Command
optVisit = Visit <$> optDir <*> (optRef <|> optTag <|> optSha)

commandVisit :: Mod CommandFields Command
commandVisit = command "visit" (info optVisit (progDesc "Reconstructs the base directory based on a previous commit"))