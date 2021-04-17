module LeGit.Set (setInfo) where

import LeGit.Basic
import LeGit.Info
import System.Exit

setInfo :: FilePath -> [(String, String)] -> IO ()
setInfo fp ss = findRepo fp >>= pom
                where pom (Just r) = changeInfo ss r
                      pom _ = putStr "Error :: "
                           >> putStr fp 
                           >> putStrLn " can't be set: not a repository!"
                           >> exitFailure
