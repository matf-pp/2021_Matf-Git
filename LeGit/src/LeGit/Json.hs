module LeGit.Json (
    -- common

    stringsToJson, jsonToStrings,

    -- for Init
    initJson,

    -- for Ignore
    readIgnores, writeIgnores,

    -- for Info
    jsonToRepoInfo, repoInfoToJson, readInfo, writeInfo
) where

import LeGit.Basic

import Text.JSON
import Data.Maybe
import Control.Applicative ((<|>))
import qualified System.IO.Strict as S
import qualified Data.HashMap.Strict as M

-- Basic JSON Stuff

    -- Reading and writing to Repo abstract
readJsonFromRepo :: (Repo -> FilePath) -> JSValue -> Repo -> IO JSValue
readJsonFromRepo f d = fmap (pom . decode) . S.readFile . f
                        where pom (Ok a) = a
                              pom _      = d

writeJsonToRepo :: (Repo -> FilePath) -> Repo -> JSValue -> IO ()
writeJsonToRepo f r = writeFile (f r) . encode

    -- Taking specific JSValue
takeJsonInt :: JSValue -> Maybe Int
takeJsonInt (JSRational _ r) = Just $ floor r
takeJsonInt _                = Nothing

takeJsonString :: JSValue -> Maybe String
takeJsonString (JSString s) = Just $ fromJSString s
takeJsonString _            = Nothing

takeJsonArray :: JSValue -> Maybe [JSValue]
takeJsonArray (JSArray a) = Just $ a
takeJsonArray _           = Nothing

takeJsonObject :: JSValue -> Maybe (M.HashMap String JSValue)
takeJsonObject (JSObject o) = Just $ M.fromList $ fromJSObject o
takeJsonObject _            = Nothing


stringsToJson :: [String] -> JSValue
stringsToJson = JSArray . map (JSString . toJSString)

jsonToStrings :: JSValue -> [String]
jsonToStrings = mapMaybe takeJsonString
              . fromMaybe [] 
              . takeJsonArray

-- Ignore Stuff

defaultIgnore :: JSValue
defaultIgnore = JSArray []

readIgnores :: Repo -> IO JSValue
readIgnores = readJsonFromRepo ignoreFile defaultIgnore
          
writeIgnores :: Repo -> JSValue -> IO () 
writeIgnores = writeJsonToRepo ignoreFile

-- Info Stuff

defaultInfo :: JSValue
defaultInfo = JSObject $ toJSObject []

readInfo :: Repo -> IO JSValue
readInfo = readJsonFromRepo infoFile defaultInfo

writeInfo :: Repo -> JSValue -> IO ()
writeInfo = writeJsonToRepo infoFile

jsonToRepoInfo :: JSValue -> M.HashMap String String
jsonToRepoInfo = M.map (fromMaybe undefined)
               . M.filter isJust 
               . M.map takeJsonString
               . fromMaybe M.empty
               . takeJsonObject

repoInfoToJson :: M.HashMap String String -> JSValue
repoInfoToJson = makeObj . map (fmap (JSString . toJSString)) . M.toList

-- Init Stuff

initJson :: Repo -> IO ()
initJson r = writeIgnores r defaultIgnore
          >> writeInfo r defaultInfo

-- Commit Stuff

initCommitJson :: IO JSValue
initCommitJson = do
        t <- getTimeString
        let pom = JSObject $ toJSObject 
                $ [("info",JSObject $ toJSObject [("time",JSString $ toJSString t)]),
                ("adds",JSArray []), ("changes",JSArray []),("removes",JSArray [])]
        return pom

infoToJson :: M.HashMap String String -> JSValue
infoToJson = JSObject . toJSObject . M.toList . M.map (JSString . toJSString)

infoFromJson :: JSValue -> Maybe (M.HashMap String String)
infoFromJson = fmap (M.map $ fromMaybe undefined . takeJsonString) . takeJsonObject

removesToJson :: [FilePath] -> JSValue
removesToJson = stringsToJson . reverse . sortPaths

removesFromJson :: JSValue -> [FilePath]
removesFromJson = jsonToStrings

diffToJson :: Diff -> JSValue
diffToJson (Remove i n) = JSObject 
                        $ toJSObject [("type", JSString $ toJSString "remove")
                                     ,("index", JSRational True $ fromIntegral i)
                                     ,("num", JSRational True $ fromIntegral n)
                                     ]
diffToJson (Add i ls)   = JSObject 
                        $ toJSObject [("type", JSString $ toJSString "add")
                                     ,("index", JSRational True $ fromIntegral i)
                                     ,("lines", stringsToJson ls)
                                     ]

diffFromJson :: JSValue -> Maybe Diff
diffFromJson js = takeJsonObject js >>= getDiff
      where getType m   = M.lookup "type" m  >>= takeJsonString
            getIndex m  = M.lookup "index" m >>= takeJsonInt
            getNum m    = M.lookup "num" m   >>= takeJsonInt
            getLines    = fmap jsonToStrings . M.lookup "lines"
            isAdd       = fmap (== "add") . getType
            isRemove    = fmap (== "remove") . getType
            getAdd m    = isAdd m >>= (Add <$> getIndex m <*> getLines m) ? Nothing
            getRemove m = isRemove m >>= (Remove <$> getIndex m <*> getNum m) ? Nothing
            getDiff m   = getAdd m <|> getRemove m

diffsToJson :: [Diff] -> JSValue
diffsToJson = JSArray . map diffToJson

diffsFromJson :: JSValue -> Maybe [Diff]
diffsFromJson js = takeJsonArray js >>= mapM diffFromJson

changeToJson :: FilePath -> [Diff] -> JSValue
changeToJson fp ds = JSObject $ toJSObject [("path", JSString $ toJSString fp)
                                           ,("changes", diffsToJson ds)]

changeFromJson :: JSValue -> Maybe (FilePath, [Diff])
changeFromJson js = takeJsonObject js >>= getChanges
      where getFilePath m = M.lookup "path" m >>= takeJsonString
            getDiffs m    = M.lookup "changes" m >>= diffsFromJson
            getChanges m  = (,) <$> getFilePath m <*> getDiffs m

changesToJson :: [(FilePath, [Diff])] -> JSValue
changesToJson = JSArray . map (uncurry changeToJson)

changesFromJson :: JSValue -> Maybe [(FilePath, [Diff])]
changesFromJson js = takeJsonArray js >>= mapM changeFromJson

addToJson :: FilePath -> Maybe [String] -> JSValue
addToJson fp = JSObject 
             . toJSObject 
             . (:) ("path", JSString $ toJSString fp)
             . maybe [] (pure . (,) "lines" . stringsToJson)

addFromJson :: JSValue -> Maybe (FilePath, Maybe [String])
addFromJson js = takeJsonObject js >>= getAdd
      where getFilePath m = M.lookup "path" m >>= takeJsonString
            getLines      = fmap jsonToStrings . M.lookup "lines"
            getAdd m      = flip (,) (getLines m) <$> getFilePath m

addsToJson :: [(FilePath, Maybe [String])] -> JSValue
addsToJson = JSArray . map (uncurry addToJson)

addsFromJson :: JSValue -> Maybe [(FilePath,  Maybe [String])]
addsFromJson js = takeJsonArray js >>= mapM addFromJson

fullDiffToJson :: M.HashMap String String 
               -> ([FilePath], [(FilePath, [Diff])], [(FilePath, Maybe [String])]) 
               -> JSValue
fullDiffToJson i (r, c, a) = JSObject 
                           $ toJSObject [ ("info", infoToJson i)
                                        , ("removes", removesToJson r)
                                        , ("changes", changesToJson c)
                                        , ("adds", addsToJson a)]