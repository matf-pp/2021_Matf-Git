module LeGit.Json (
    -- common
    stringsToJson, stringsFromJson, stringMapToJson, stringMapToJson', stringMapFromJson,

    -- for Init
    initJson,

    -- for Ignore
    readIgnores, writeIgnores,

    -- for Info
    readInfo, writeInfo,

    -- for Tree
    readTree, writeTree
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
stringsToJson = showJSON

stringsFromJson :: JSValue -> [String]
stringsFromJson = mapMaybe takeJsonString
              . fromMaybe [] 
              . takeJsonArray

stringMapFromJson :: JSValue -> M.HashMap String String
stringMapFromJson = M.mapMaybe takeJsonString
                  . fromMaybe M.empty
                  . takeJsonObject

stringMapToJson :: [(String, String)] -> JSValue
stringMapToJson = makeObj . map (fmap showJSON)

stringMapToJson' :: M.HashMap String String -> JSValue
stringMapToJson' = stringMapToJson . M.toList

-- Ignore Stuff

defaultIgnore :: JSValue
defaultIgnore = JSArray []

readIgnores :: Repo -> IO JSValue
readIgnores = readJsonFromRepo ignoreFile defaultIgnore
          
writeIgnores :: Repo -> JSValue -> IO () 
writeIgnores = writeJsonToRepo ignoreFile

-- Info Stuff

defaultInfo :: JSValue
defaultInfo = makeObj []

readInfo :: Repo -> IO JSValue
readInfo = readJsonFromRepo infoFile defaultInfo

writeInfo :: Repo -> JSValue -> IO ()
writeInfo = writeJsonToRepo infoFile

-- Init Stuff

initJson :: Repo -> IO ()
initJson r = writeIgnores r defaultIgnore
          >> writeInfo r defaultInfo

-- Tree Stuff
defaultTree :: JSValue
defaultTree = makeObj []

readTree :: Repo -> IO JSValue
readTree = readJsonFromRepo treeFile defaultTree

writeTree :: Repo -> JSValue -> IO ()
writeTree = writeJsonToRepo treeFile

-- Pointer Stuff

defaultHead' :: Head
defaultHead' = Ref "main"

defaultHead :: JSValue
defaultHead = headToJson defaultHead'

headToJson :: Head -> JSValue
headToJson (Ref p) = stringMapToJson [("type", "ref"), ("value", p)]
headToJson (Tag p) = stringMapToJson [("type", "tag"), ("value", p)]
headToJson (Sha p) = stringMapToJson [("type", "sha"), ("value", p)]

headFromJson :: JSValue -> Maybe Head
headFromJson js = takeJsonObject js >>= getPointer
      where getPointer m = getRef m <|> getTag m <|> getSha m
            getType m    = M.lookup "type" m   >>= takeJsonString
            getValue m   = M.lookup "value" m  >>= takeJsonString
            isRef        = fmap (== "ref") . getType
            isTag        = fmap (== "tag") . getType
            isSha        = fmap (== "Sha") . getType
            getRef m     = isRef m >>= Ref <$> getValue m ? Nothing
            getTag m     = isTag m >>= Tag <$> getValue m ? Nothing
            getSha m     = isSha m >>= Sha <$> getValue m ? Nothing

-- Commit Stuff

initCommitJson :: IO JSValue
initCommitJson = flip fullDiffToJson ([],[],[]) . M.singleton "time" <$> getTimeString

removesToJson :: [FilePath] -> JSValue
removesToJson = stringsToJson . reverse . sortPaths

removesFromJson :: JSValue -> [FilePath]
removesFromJson = stringsFromJson

diffToJson :: Diff -> JSValue
diffToJson (Remove i n) = makeObj [("type", showJSON "remove")
                                  ,("index", showJSON i)
                                  ,("num", showJSON n)]
diffToJson (Add i ls)   = makeObj [("type", showJSON "add")
                                  ,("index", showJSON i)
                                  ,("lines", stringsToJson ls)]

diffFromJson :: JSValue -> Maybe Diff
diffFromJson js = takeJsonObject js >>= getDiff
      where getType m   = M.lookup "type" m  >>= takeJsonString
            getIndex m  = M.lookup "index" m >>= takeJsonInt
            getNum m    = M.lookup "num" m   >>= takeJsonInt
            getLines    = fmap stringsFromJson . M.lookup "lines"
            isAdd       = fmap (== "add") . getType
            isRemove    = fmap (== "remove") . getType
            getAdd m    = isAdd m >>= Add <$> getIndex m <*> getLines m ? Nothing
            getRemove m = isRemove m >>= Remove <$> getIndex m <*> getNum m ? Nothing
            getDiff m   = getAdd m <|> getRemove m

diffsToJson :: [Diff] -> JSValue
diffsToJson = JSArray . map diffToJson

diffsFromJson :: JSValue -> Maybe [Diff]
diffsFromJson js = takeJsonArray js >>= mapM diffFromJson

changeToJson :: FilePath -> [Diff] -> JSValue
changeToJson fp ds = makeObj [("path", showJSON fp), ("changes", diffsToJson ds)]

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
addToJson fp = makeObj . (:) ("path", showJSON fp)
             . maybe [] (pure . (,) "lines" . stringsToJson)

addFromJson :: JSValue -> Maybe (FilePath, Maybe [String])
addFromJson js = takeJsonObject js >>= getAdd
      where getFilePath m = M.lookup "path" m >>= takeJsonString
            getLines      = fmap stringsFromJson . M.lookup "lines"
            getAdd m      = flip (,) (getLines m) <$> getFilePath m

addsToJson :: [(FilePath, Maybe [String])] -> JSValue
addsToJson = JSArray . map (uncurry addToJson)

addsFromJson :: JSValue -> Maybe [(FilePath,  Maybe [String])]
addsFromJson js = takeJsonArray js >>= mapM addFromJson

fullDiffToJson :: M.HashMap String String 
               -> ([FilePath], [(FilePath, [Diff])], [(FilePath, Maybe [String])]) 
               -> JSValue
fullDiffToJson i (r, c, a) = makeObj [ ("info", stringMapToJson' i)
                                     , ("removes", removesToJson r)
                                     , ("changes", changesToJson c)
                                     , ("adds", addsToJson a)]
