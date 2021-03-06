module LeGit.Init (LeGit.Init.init) where

import LeGit.Basic
import LeGit.Pointers (initState)

import System.Directory

errorDirCheck :: (FilePath -> IO Bool) -> FilePath -> String -> IO ()
errorDirCheck cond dir msg =  cond dir
            >>= errorMsg (dir ++ " can't be initialized: " ++ msg) ? return ()

initRepo :: Repo -> IO ()
initRepo r = mapM_ (createDirectory . ($r)) [repoDir, commitsDir]
          >> initState r

deleteRepo :: Repo -> IO ()
deleteRepo r = putStrLn ("Deleting repo: " ++ baseDir r) >> removeDirectoryRecursive (repoDir r)

initForce :: FilePath -> IO ()
initForce fp = do
    listRepos fp >>= mapM_ deleteRepo
    findRepo fp >>= maybe (return ()) deleteRepo
    initRepo $ fromBaseDir fp

initSoft :: FilePath -> IO ()
initSoft fp = errorDirCheck isRepo fp "already a repository"
           >> errorDirCheck hasRepos fp "contains a repository"
           >> initRepo (fromBaseDir fp)

init :: FilePath -> Bool -> IO ()
init dir f = do
    errorDirCheck (fmap not . doesDirectoryExist) dir "not a directory"
    putStrLn $ "Initializing " ++ dir 
    (initForce ? initSoft) f dir
    putStrLn "Done!"
