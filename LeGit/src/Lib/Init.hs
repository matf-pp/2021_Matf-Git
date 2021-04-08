module Lib.Init (Lib.Init.init) where

import Lib.Basic

import System.Directory
import System.FilePath
import System.Exit

errorDirCheck :: (FilePath -> IO Bool) -> FilePath -> String -> IO ()
errorDirCheck cond dir msg =  cond dir
            >>= (putStr "Error :: " >> putStr dir >> putStr " can't be initialized: " >> putStr msg 
            >> putChar '!' >> exitFailure) ? return ()

initRepo :: Repo -> IO ()
initRepo = mapM_ createDirectory <$> sequenceA [repoDir, infoDir]

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
