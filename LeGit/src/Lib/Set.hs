module Lib.Set (setUsername, setEmail) where

import Lib.Basic
import System.Directory
import System.Exit
import System.FilePath

setDir :: String -> IO FilePath
setDir dirStr = if null dirStr
                then getCurrentDirectory
                else makeAbsolute dirStr

writeRepoInfo :: (Repo -> FilePath) -> String -> String -> IO ()
writeRepoInfo f fp s = setDir fp >>= findRepo >>= (\d -> pom f d s)
                where pom f Nothing s = putStrLn (str fp) >> exitFailure
                            where str fp = "Error :: " ++ fp ++ " can't be set: not a repository!"
                      pom f (Just r) s = writeFile (f r) s

setUsername :: String -> String -> IO ()
setUsername = writeRepoInfo usernameFile

setEmail :: String -> String -> IO ()
setEmail = writeRepoInfo emailFile