module LeGit.Show (showInfo, showIgnores) where

import LeGit.Basic
import LeGit.Ignore
import LeGit.Info

import System.Exit
import Data.Maybe

showErrorCheck :: (Repo -> IO ()) -> FilePath -> IO ()
showErrorCheck f fp = findRepo fp >>= pom
    where pom (Just r) = f r
          pom Nothing  = putStr "Error :: "
                      >> putStr fp 
                      >> putStrLn " can't be shown: not a repository!"
                      >> exitFailure

showInfo :: FilePath -> IO ()
showInfo fp = showErrorCheck f fp
    where f r = mapM_ (showSingleInfo r) infoFields
          showSingleInfo r s = putStr s >> putStr ": "
                            >> (fmap (fromMaybe "Not set!") $ getInfo s r) >>= putStrLn

showIgnores :: FilePath -> IO ()
showIgnores fp = showErrorCheck f fp
    where f r = putStrLn "Ignores:" >> getIgnores r >>= mapM_ putStrLn