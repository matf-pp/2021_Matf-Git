module LeGit.Ignore (getIgnores, addIgnoreToRepo, removeIgnoreFromRepo) where

import LeGit.Basic

import System.FilePath

getIgnores :: Repo -> IO [FilePath]
getIgnores = readJsonFromRepo ignoreFile []

insertFilePath :: [FilePath] -> FilePath -> [FilePath]
insertFilePath [] fp = [fp]
insertFilePath list@(x:xs) fp
    | isParent x fp = list
    | isParent fp x = fp : filter (not . isParent fp) xs
    | otherwise     = x : insertFilePath xs fp

deleteFilePath :: [FilePath] -> FilePath -> [FilePath]
deleteFilePath xs fp = filter (not . isParent fp) xs

makeRelativeToBaseDir :: Repo -> FilePath -> FilePath
makeRelativeToBaseDir r = makeRelative (baseDir r)

absIgnoreRepo :: ([FilePath] -> FilePath -> [FilePath]) -> Repo -> FilePath -> IO ()
absIgnoreRepo f rep fp = flip f (makeRelativeToBaseDir rep fp) 
                     <$> getIgnores rep 
                     >>= writeJsonToRepo ignoreFile rep
-- (>>=) :: ((<$>) :: ((.) :: [FilePath] -> JSValue) -> IO [FilePath]) -> IO JSValue) -> IO ()

addIgnoreToRepo :: Repo -> FilePath -> IO ()
addIgnoreToRepo = absIgnoreRepo insertFilePath


removeIgnoreFromRepo :: Repo -> FilePath -> IO ()
removeIgnoreFromRepo = absIgnoreRepo deleteFilePath
