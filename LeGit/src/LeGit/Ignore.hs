module LeGit.Ignore (getIgnores) where

import LeGit.Basic
import Text.JSON

readIgnores :: Repo -> IO JSValue
readIgnores = fmap (pom . decode) . readFile . ignoreFile
    where pom (Ok rez) = rez
          pom _ = JSArray []
          
writeIgnores :: Repo -> JSValue -> IO () 
writeIgnores r = writeFile (ignoreFile r) . encode

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