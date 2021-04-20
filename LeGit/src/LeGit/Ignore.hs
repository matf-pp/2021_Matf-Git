module LeGit.Ignore (defaultIgnore, writeIgnores, getIgnores, addIgnoreToRepo, removeIgnoreFromRepo) where

import LeGit.Basic
import Text.JSON
import System.FilePath

defaultIgnore :: JSValue
defaultIgnore = JSArray []

readIgnores :: Repo -> IO JSValue
readIgnores = readJsonFromRepo ignoreFile defaultIgnore
          
writeIgnores :: Repo -> JSValue -> IO () 
writeIgnores = writeJsonToRepo ignoreFile

jsonToFilePaths :: JSValue -> [FilePath]
jsonToFilePaths = jsonToStrings

filePathsToJson :: [FilePath] -> JSValue
filePathsToJson = stringsToJson

getIgnores :: Repo -> IO [FilePath]
getIgnores = fmap jsonToFilePaths . readIgnores

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
absIgnoreRepo f rep fp = filePathsToJson 
                       . flip f (makeRelativeToBaseDir rep fp) 
                     <$> getIgnores rep 
                     >>= writeIgnores rep
-- (>>=) :: ((<$>) :: ((.) :: [FilePath] -> JSValue) -> IO [FilePath]) -> IO JSValue) -> IO ()

addIgnoreToRepo :: Repo -> FilePath -> IO ()
addIgnoreToRepo = absIgnoreRepo insertFilePath


removeIgnoreFromRepo :: Repo -> FilePath -> IO ()
removeIgnoreFromRepo = absIgnoreRepo deleteFilePath