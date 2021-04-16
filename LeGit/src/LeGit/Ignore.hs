module LeGit.Ignore (defaultIgnore , writeIgnores, getIgnores) where

import LeGit.Basic
import Text.JSON

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

-- showIgnore :: FilePath -> IO ()
-- showIgnore fp = findRepo fp >>= pom
--     where pom (Just r) = showRepoUserInfo r
--           pom Nothing  = putStr "Error :: "
--                       >> putStr fp 
--                       >> putStrLn " can't be shown: not a repository!"
--                       >> exitFailure