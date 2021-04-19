module LeGit.Ignore (defaultIgnore, writeIgnores, getIgnores, addIgnoreToRepo, removeIgnoreFromRepo) where

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
    where isParent  = on isPrefixOf (map dropTrailingPathSeparator . splitPath)

deleteFilePath :: [FilePath] -> FilePath -> [FilePath]
deleteFilePath xs fp = filter (isNotParent fp) xs
    where isNotParent  = fmap not . on isPrefixOf (map dropTrailingPathSeparator . splitPath)

makeRelativeToBaseDir :: Repo -> FilePath -> FilePath
makeRelativeToBaseDir r = makeRelative (baseDir r)

addIgnoreToRepo :: Repo -> FilePath -> IO ()
addIgnoreToRepo rep fp = filePathsToJson 
                       . flip insertFilePath (makeRelativeToBaseDir rep fp) 
                     <$> getIgnores rep 
                     >>= writeIgnores rep
-- (>>=) :: ((<$>) :: ((.) :: [FilePath] -> JSValue) -> IO [FilePath]) -> IO JSValue) -> IO ()

removeIgnoreFromRepo :: Repo -> FilePath -> IO ()
removeIgnoreFromRepo rep fp = filePathsToJson 
                            . flip deleteFilePath (makeRelativeToBaseDir rep fp) 
                          <$> getIgnores rep 
                          >>= writeIgnores rep
-- (>>=) :: ((<$>) :: ((.) :: [FilePath] -> JSValue) -> IO [FilePath]) -> IO JSValue) -> IO ()
