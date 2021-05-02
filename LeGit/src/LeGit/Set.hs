module LeGit.Set (
      setUserName, setEmail, 
      addIgnore, removeIgnore,
      addRef, addTag
) where

import LeGit.Basic
import LeGit.Info
import LeGit.Ignore
import LeGit.Pointers

setErrorCheck :: (Repo -> IO ()) -> FilePath -> IO ()
setErrorCheck f fp = findRepo fp >>= pom
    where pom (Just r) = f r
          pom Nothing  = errorMsg $ fp ++ " can't be set: not a repository"

setUserName :: FilePath -> String -> IO ()
setUserName fp s = setErrorCheck f fp
      where f r = changeInfo ("username", s) r

setEmail :: FilePath -> String -> IO ()
setEmail fp s = setErrorCheck f fp
      where f r = changeInfo ("email", s) r

addIgnore :: FilePath -> FilePath -> IO ()
addIgnore repo fp = setErrorCheck f repo
      where f = flip addIgnoreToRepo fp

removeIgnore :: FilePath -> FilePath -> IO ()
removeIgnore repo fp = setErrorCheck f repo
      where f = flip removeIgnoreFromRepo fp

addRef :: FilePath -> String -> IO ()
addRef fp s = setErrorCheck pom fp
      where pom r = setRef r s

addTag :: FilePath -> String -> IO ()
addTag fp s = setErrorCheck pom fp
      where pom r = setTag r s