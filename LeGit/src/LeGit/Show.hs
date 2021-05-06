module LeGit.Show (showInfo, showIgnores, showHead) where

import LeGit.Basic
import LeGit.Ignore
import LeGit.Info
import LeGit.Pointers

import Data.Maybe

showErrorCheck :: (Repo -> IO ()) -> FilePath -> IO ()
showErrorCheck f fp = findRepo fp >>= pom
    where pom (Just r) = f r
          pom Nothing  = errorMsg $ fp ++ " can't be set: not a repository"

showInfo :: FilePath -> IO ()
showInfo = showErrorCheck f
    where f r = mapM_ (showSingleInfo r) infoFields
          showSingleInfo r s = putStr s >> putStr ": "
                            >> fmap (fromMaybe "Not set!") (getInfo s r) >>= putStrLn

showIgnores :: FilePath -> IO ()
showIgnores = showErrorCheck f
    where f r = putStrLn "Ignores:" >> getIgnores r >>= mapM_ putStrLn

showHead :: FilePath -> IO ()
showHead = showErrorCheck f
    where f r = getPointers r >>= print . phead