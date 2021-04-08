module Main where
import Lib

import Options.Applicative
import System.Directory

data SetType = UserName String
             | Email String
  deriving (Eq, Show)

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

data Command = Greet
             | Init {directory :: String, force :: Bool}
             | Set  {directory :: String, setArgs :: [SetType]}
  deriving (Eq, Show)

optDir :: Parser String
optDir = strOption (long "directory"
                   <> short 'd' 
                   <> value ""
                   <> metavar "PATH" 
                   <> help "Path to directory to be initialised into a repository (default: current working directory)")

optForce :: Parser Bool
optForce = switch (long "force"
                  <> short 'f'
                  <> help "Forces initialization (may owerwrite previous repo info and delete files named after required repo info files)")

initOptions :: Parser Command
initOptions = Init <$> optDir <*> optForce

setOptions :: Parser Command
setOptions = Set <$> optDir <*> some (optEmail <|> optUserName)


optDirIstance :: String -> IO FilePath
optDirIstance dirStr = if null dirStr
                     then getCurrentDirectory
                     else makeAbsolute dirStr

optLeGit :: Parser Command
optLeGit = subparser (
              command "greet" (info (pure Greet) (progDesc "Print greeting"))
           <> command "init" (info initOptions (progDesc "Initialises a directory into a LeGit repo"))
           <> command "set" (info setOptions (progDesc "Sets repository specific information"))
         )

opts :: ParserInfo Command
opts = info (optLeGit <**> helper) (fullDesc
        <> header "LeGit - a SVC implemented in Haskell (somewhat based on git)" )

        
run :: Command -> IO ()
run Greet = putStrLn "Hi!"
run (Init d f) = optDirIstance d >>= flip Lib.init f
run (Set d args) = optDirIstance d >>= (\dir -> mapM_ (pom dir) args)
    where pom r (UserName u) = Lib.setUsername r u
          pom r (Email e) = Lib.setEmail r e

main :: IO ()
main = execParser opts >>= run