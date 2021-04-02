module Lib.Init (Lib.Init.init) where

import Lib.Basic

import System.Directory
import System.FilePath
import System.Exit

errorCheck :: IO Bool -> String -> String -> IO ()
errorCheck cond dir msg = do
        b <- cond
        if b then putStrLn ("Error:: " ++ dir ++ " can't be initialized: " ++ msg ++ "!")
               >> exitFailure
             else return ()

initDir :: String -> IO FilePath
initDir dirStr = if null dirStr
                 then getCurrentDirectory
                 else return dirStr

initRepo :: FilePath -> IO ()
initRepo path = do
    let repo = joinPath [path, repoDirName]
    createDirectory repo 

initForce :: FilePath -> IO ()
initForce fp = return ()

initSoft :: FilePath -> IO ()
initSoft fp = do
    errorCheck (hasRepos fp) fp "contains a repository"
    errorCheck (isRepo fp) fp "already a repository"
    initRepo fp

init :: FilePath -> Bool -> IO ()
init d f = do
    dir <- initDir d
    errorCheck (not <$> doesDirectoryExist dir) dir "not a directory"
    putStrLn $ "Initializing " ++ dir 
    (if f then initForce else initSoft) dir
    putStrLn "Done!"
