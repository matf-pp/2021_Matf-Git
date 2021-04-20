module LeGit.Info (changeInfo, getInfo, writeInfo, infoFields, defaultInfo, getUserNameAssert) where

import LeGit.Basic

import Text.JSON
import Data.Maybe
import qualified Data.HashMap.Strict as M

type RepoInfo = M.HashMap String String

infoFields :: [String]
infoFields = ["username", "email"]

defaultInfo :: JSValue
defaultInfo = JSObject $ toJSObject []

readInfo :: Repo -> IO JSValue
readInfo = readJsonFromRepo infoFile defaultInfo

writeInfo :: Repo -> JSValue -> IO ()
writeInfo = writeJsonToRepo infoFile

jsonToRepoInfo :: JSValue -> RepoInfo
jsonToRepoInfo = M.map (fromMaybe undefined)
               . M.filter isJust 
               . M.map takeJsonString
               . fromMaybe M.empty
               . takeJsonObject

repoInfoToJson :: RepoInfo -> JSValue
repoInfoToJson = makeObj . map (fmap (JSString . toJSString)) . M.toList

getInfo :: String -> Repo -> IO (Maybe String)
getInfo s = fmap (M.lookup s) . fmap jsonToRepoInfo . readInfo

insertRepoInfo :: (String, String) -> RepoInfo -> RepoInfo
insertRepoInfo = uncurry M.insert

changeInfo :: (String, String) -> Repo ->  IO ()
changeInfo ss r = jsonToRepoInfo <$> readInfo r >>= writeInfo r . repoInfoToJson . insertRepoInfo ss


getUserNameAssert :: Repo -> IO String
getUserNameAssert = fmap (fromMaybe $ error "Error :: usenrame not set!") 
                  . getInfo "username"

--insertRepoInfo :: RepoInfo -> (String, String) -> RepoInfo