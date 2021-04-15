module LeGit.Show (showInfo, showIgnores) where

import LeGit.Basic
import LeGit.Ignore

import System.Directory
import System.Exit

showRepoUserInfo :: Repo -> IO ()                                   
showRepoUserInfo r = putStr "username: " >> pom usernameFile
                  >> putStr "email: " >> pom emailFile
    where pom f = doesFileExist (f r) >>= (readFile (f r) >>= putStrLn) ? (putStrLn "not set")

showInfo :: FilePath -> IO ()
showInfo fp = findRepo fp >>= pom
    where pom (Just r) = showRepoUserInfo r
          pom Nothing  = putStr "Error :: "
                      >> putStr fp 
                      >> putStrLn " can't be shown: not a repository!"
                      >> exitFailure

showIgnores :: FilePath -> IO ()
showIgnores fp = putStrLn "Ignores:" 
             >> (getIgnores $ fromBaseDir fp )
            >>= mapM_ putStrLn
