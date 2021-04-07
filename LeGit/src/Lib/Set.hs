module Lib.Set (setUsername, setEmail) where

import Lib.Basic
import System.Directory
import System.Exit
import System.FilePath

writeRepoInfo :: (Repo -> FilePath) -> FilePath -> String -> IO ()
writeRepoInfo f fp s = findRepo fp >>= pom 
                where pom (Just r) = writeFile (f r) s
                      pom _ = putStr "Error :: "
                           >> putStr fp 
                           >> putStrLn " can't be set: not a repository!"
                           >> exitFailure
                      

setUsername :: FilePath -> String -> IO ()
setUsername = writeRepoInfo usernameFile

setEmail :: FilePath -> String -> IO ()
setEmail = writeRepoInfo emailFile