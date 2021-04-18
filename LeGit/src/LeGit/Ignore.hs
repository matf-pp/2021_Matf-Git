module LeGit.Ignore (defaultIgnore, writeIgnores, getIgnores, addIgnores, removeIgnores) where

import LeGit.Basic
import Text.JSON
import System.FilePath
import Data.List
import Data.Function

defaultIgnore :: JSValue
defaultIgnore = JSArray []

readIgnores :: Repo -> IO JSValue
readIgnores = readJsonFromRepo ignoreFile defaultIgnore
          
writeIgnores :: Repo -> JSValue -> IO () 
writeIgnores = writeJsonToRepo ignoreFile

jsonToFilePaths :: JSValue -> [FilePath]
jsonToFilePaths (JSArray xs) = filter (not . null) $ map pom xs
    where pom (JSString s) = fromJSString s
          pom _ = ""
jsonToFilePaths _ = []

filePathsToJson :: [FilePath] -> JSValue
filePathsToJson = showJSONs

getIgnores :: Repo -> IO [FilePath]
getIgnores = fmap jsonToFilePaths . readIgnores

insertFilePath :: [FilePath] -> FilePath -> [FilePath]
insertFilePath [] fp = [fp]
insertFilePath list@(x:xs) fp
    | isParent x fp = list
    | isParent fp x = fp : filter (not . isParent fp) xs
    | otherwise     = x : insertFilePath xs fp
    where isParent  = on isPrefixOf splitPath

deleteFilePath :: [FilePath] -> FilePath -> [FilePath]
deleteFilePath xs fp = filter (isNotParent fp) xs
    where isNotParent  = fmap not . on isPrefixOf splitPath

addIgnores :: Repo -> [FilePath] -> IO ()
addIgnores rep fps = filePathsToJson . flip (foldl insertFilePath) fps <$> getIgnores rep >>= writeIgnores rep
-- (>>=) :: ((<$>) :: ((.) :: [FilePath] -> JSValue) -> IO [FilePath]) -> IO JSValue) -> IO ()

removeIgnores :: Repo -> [FilePath] -> IO ()
removeIgnores rep fps = filePathsToJson . flip (foldl deleteFilePath) fps <$> getIgnores rep >>= writeIgnores rep
-- (>>=) :: ((<$>) :: ((.) :: [FilePath] -> JSValue) -> IO [FilePath]) -> IO JSValue) -> IO ()
