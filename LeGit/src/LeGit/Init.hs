module LeGit.Init (LeGit.Init.init) where

import LeGit.Basic
import LeGit.Ignore
import LeGit.Info

import System.Directory

errorDirCheck :: (FilePath -> IO Bool) -> FilePath -> String -> IO ()
errorDirCheck cond dir msg =  cond dir
            >>= (error ("Error :: " ++ dir ++ " can't be initialized: " ++ msg ++ "!")) ? return ()

initRepo :: Repo -> IO ()
initRepo r = mapM_ createDirectory (map ($r) directories)
            >> writeIgnores r defaultIgnore
            >> writeInfo r defaultInfo
            where directories = [repoDir, pointersDir, refsDir, tagsDir, objectsDir, commitsDir]

deleteRepo :: Repo -> IO ()
deleteRepo r = putStrLn ("Deleting repo: " ++ (baseDir r)) >> removeDirectoryRecursive (repoDir r)

initForce :: FilePath -> IO ()
initForce fp = do
    listRepos fp >>= mapM_ deleteRepo
    findRepo fp >>= deleteMaybeRepo
    initRepo (fromBaseDir fp)
        where deleteMaybeRepo Nothing  = return ()
              deleteMaybeRepo (Just r) = deleteRepo r

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
