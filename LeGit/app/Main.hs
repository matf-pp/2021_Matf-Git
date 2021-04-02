module Main where
import Lib

import Data.List
import Data.Monoid
import Options.Applicative


data Command = Greet
             | Init {directory :: String, force :: Bool}
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

optLeGit :: Parser Command
optLeGit = subparser (
              command "greet" (info (pure Greet) (progDesc "Print greeting"))
           <> command "init" (info initOptions (progDesc "Initialises a directory into a LeGit repo"))
         )

run :: Command -> IO ()
run (Init d f) = Lib.init d f
run Greet = putStrLn "Hi!"

opts :: ParserInfo Command
opts = info (optLeGit <**> helper) (fullDesc
        <> header "LeGit - a SVC implemented in Haskell (somewhat based on git)" )

main :: IO ()
main = execParser opts >>= run