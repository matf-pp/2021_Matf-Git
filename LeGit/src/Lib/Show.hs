module Lib.Show (showInfo) where

import Lib.Basic
import System.Directory
import System.Exit
import System.FilePath

showRepoUserInfo :: Repo -> IO ()                                   
showRepoUserInfo r = putStr "username: " >> pom usernameFile
                  >> putStr "email: " >> pom emailFile
    where pom f = doesFileExist (f r) 
                   >>= (\b -> if b then readFile (f r) >>= putStrLn
                                   else putStrLn "not set") 

showInfo :: FilePath -> IO ()
showInfo fp = findRepo fp >>= pom
    where pom (Just r) = showRepoUserInfo r
          pom Nothing  = putStr "Error :: "
                      >> putStr fp 
                      >> putStrLn " can't be shown: not a repository!"
                      >> exitFailure
