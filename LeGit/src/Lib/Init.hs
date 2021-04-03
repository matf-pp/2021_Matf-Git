module Lib.Init (Lib.Init.init) where

import Lib.Basic

import System.Directory
import System.FilePath
import System.Exit

erorDirCheck :: (FilePath -> IO Bool) -> FilePath -> String -> IO ()
erorDirCheck cond dir msg = do
        b <- cond dir
        if b then putStrLn ("Error :: " ++ dir ++ " can't be initialized: " ++ msg ++ "!")
               >> exitFailure
             else return ()

initDir :: String -> IO FilePath
initDir dirStr = if null dirStr
                 then getCurrentDirectory
                 else makeAbsolute dirStr

initRepo :: Repo -> IO ()
initRepo repo = mapM_ createDirectory [f repo | f <- [repoDir]]

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
initSoft fp = erorDirCheck isRepo fp "already a repository"
           >> erorDirCheck hasRepos fp "contains a repository"
           >> initRepo (fromBaseDir fp)

init :: FilePath -> Bool -> IO ()
init d f = do
    dir <- initDir d
    erorDirCheck (\d -> not <$> doesDirectoryExist d) dir "not a directory"
    putStrLn $ "Initializing " ++ dir 
    (if f then initForce else initSoft) dir
    putStrLn "Done!"
