module LeGit.Set (setUserName, setEmail, addIgnore, removeIgnore) where

import LeGit.Basic
import LeGit.Info
import LeGit.Ignore
import System.Exit

setErrorCheck :: (Repo -> IO ()) -> FilePath -> IO ()
setErrorCheck f fp = findRepo fp >>= pom
    where pom (Just r) = f r
          pom Nothing  = putStr "Error :: "
                      >> putStr fp 
                      >> putStrLn " can't be set: not a repository!"
                      >> exitFailure

setUserName :: FilePath -> String -> IO ()
setUserName fp s = setErrorCheck f fp
      where f r = changeInfo ("username", s) r

setEmail :: FilePath -> String -> IO ()
setEmail fp s = setErrorCheck f fp
      where f r = changeInfo ("email", s) r

addIgnore :: FilePath -> FilePath -> IO ()
addIgnore repo fp = setErrorCheck f repo
      where f r = flip addIgnoreToRepo fp r

removeIgnore :: FilePath -> FilePath -> IO ()
removeIgnore repo fp = setErrorCheck f repo
      where f r = flip removeIgnoreFromRepo fp r