module LeGit.Info (getInfo, infoFields, getUserNameAssert, changeInfo) where

import LeGit.Basic

import Data.Maybe
import qualified Data.HashMap.Strict as M

type RepoInfo = M.HashMap String String

infoFields :: [String]
infoFields = ["username", "email"]

readInfo :: Repo -> IO RepoInfo
readInfo = readJsonFromRepo infoFile M.empty

getInfo :: String -> Repo -> IO (Maybe String)
getInfo s = fmap (M.lookup s) . readInfo

insertRepoInfo :: (String, String) -> RepoInfo -> RepoInfo
insertRepoInfo = uncurry M.insert

changeInfo :: (String, String) -> Repo ->  IO ()
changeInfo ss r = readInfo r
              >>= writeJsonToRepo infoFile r . insertRepoInfo ss


getUserNameAssert :: Repo -> IO String
getUserNameAssert = fmap (fromMaybe $ error "Error :: usenrame not set!") 
                  . getInfo "username"

--insertRepoInfo :: RepoInfo -> (String, String) -> RepoInfo