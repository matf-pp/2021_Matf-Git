module LeGit.Info (changeInfo, getInfo, writeInfo, infoFields, defaultInfo) where

import LeGit.Basic

import Text.JSON
import Data.Maybe
import qualified Data.Map as M

type RepoInfo = M.Map String String

infoFields :: [String]
infoFields = ["username", "email"]

defaultInfo :: JSValue
defaultInfo = JSObject $ toJSObject []

readInfo :: Repo -> IO JSValue
readInfo = readJsonFromRepo infoFile defaultInfo

writeInfo :: Repo -> JSValue -> IO ()
writeInfo = writeJsonToRepo infoFile

jsonToRepoInfo :: JSValue -> RepoInfo
jsonToRepoInfo = M.fromList 
               . map (fmap (fromMaybe undefined)) 
               . filter (isJust . snd) 
               . map (fmap pomMS) 
               . pomRes 
               . (decJSDict "" :: JSValue -> Result [(String, JSValue)])
                where pomRes (Ok a) = a
                      pomRes _      = []
                      pomMS (JSString a) = Just $ fromJSString a
                      pomMS _            = Nothing

repoInfoToJson :: RepoInfo -> JSValue
repoInfoToJson = makeObj . map (fmap (JSString . toJSString)) . M.toList

getInfo :: String -> Repo -> IO (Maybe String)
getInfo s = fmap (M.lookup s) . fmap jsonToRepoInfo . readInfo

insertRepoInfo :: (String, String) -> RepoInfo -> RepoInfo
insertRepoInfo = uncurry M.insert

changeInfo :: [(String, String)] -> Repo ->  IO ()
changeInfo ss r = (fmap jsonToRepoInfo (readInfo r)) >>= (writeInfo r . repoInfoToJson . flip (foldr insertRepoInfo) ss)




--insertRepoInfo :: RepoInfo -> (String, String) -> RepoInfo