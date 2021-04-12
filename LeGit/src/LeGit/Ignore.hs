module LeGit.Ignore () where

import LeGit.Basic
import Text.JSON

readIgnore :: Repo -> IO JSValue
readIgnore r = pom . decode <$> readFile (ignoreFile r)
    where pom (Ok rez) = rez
          pom _ = JSArray []
          
writeIgnore :: Repo -> JSValue -> IO () 
writeIgnore r js = writeFile (ignoreFile r) (encode js)

jsonToFilePaths :: JSValue -> [FilePath]
jsonToFilePaths (JSArray xs) = filter (not . null) $ map pom xs
    where pom (JSString s) = fromJSString s
          pom _ = ""
jsonToFilePaths _ = []

filePathsToJson :: [FilePath] -> JSValue
filePathsToJson = showJSONs

-- showIgnore :: FilePath -> IO ()
-- showIgnore fp = findRepo fp >>= pom
--     where pom (Just r) = showRepoUserInfo r
--           pom Nothing  = putStr "Error :: "
--                       >> putStr fp 
--                       >> putStrLn " can't be shown: not a repository!"
--                       >> exitFailure