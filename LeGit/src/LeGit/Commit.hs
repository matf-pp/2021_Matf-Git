module LeGit.Commit (nameGen,readFileLines,makeDiff,makeFilePathDiff,genFilePaths,initCommitJson) where

import Text.JSON
import Crypto.Hash.SHA256
import Text.Hex
import Data.Function
import Data.Maybe
import System.Directory
import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as B
import qualified Data.Algorithm.Diff as D
import qualified Data.HashMap.Strict as M
import Control.Applicative ((<|>))
import System.FilePath.Find
import System.FilePath

import LeGit.Basic
import LeGit.Info
import LeGit.Ignore (getIgnores)

initCommitJson :: IO JSValue
initCommitJson = do
        t <- getTimeString
        let pom = JSObject $ toJSObject 
                $ [("info",JSObject $ toJSObject [("time",JSString $ toJSString t)]),
                ("adds",JSArray []), ("changes",JSArray []),("removes",JSArray [])]
        return pom        
        

infoToJson :: Repo -> IO JSValue
infoToJson r = do
      u <- getUserNameAssert r
      e <- getInfo "email" r
      t <- getTimeString
      let email = ([("email", fromMaybe undefined e)] ? []) $ isJust e
      return $ JSObject $ toJSObject $ map (fmap (JSString . toJSString)) (("username", u):("time", t):email)

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

changeToJson :: FilePath -> IO JSValue      --TODO
changeToJson = undefined                    --TODO

changeFromJson :: JSValue -> Maybe (FilePath, [Diff])
changeFromJson js = takeJsonObject js >>= getChanges
      where getFilePath m = M.lookup "path" m >>= takeJsonString
            getDiffs m    = M.lookup "changes" m >>= diffsFromJson
            getChanges m  = (,) <$> getFilePath m <*> getDiffs m

changesToJson :: [FilePath] -> IO JSValue   --TODO
changesToJson _ = return $ JSArray []       --TODO

changesFromJson :: JSValue -> Maybe [(FilePath, [Diff])]
changesFromJson js = takeJsonArray js >>= mapM changeFromJson

addToJson :: FilePath -> IO JSValue
addToJson fp = do
      b <- doesFileExist fp
      l <- (stringsToJson  <$> readFileLines fp ? undefined) b
      let res = ([("lines", l)] ? []) b
      return $ JSObject $ toJSObject $ ("path", JSString $ toJSString fp):res

addFromJson :: JSValue -> Maybe (FilePath, Maybe [String])
addFromJson js = takeJsonObject js >>= getAdd
      where getFilePath m = M.lookup "path" m >>= takeJsonString
            getLines      = fmap jsonToStrings . M.lookup "lines"
            getAdd m      = flip (,) (getLines m) <$> getFilePath m

addsToJson :: [FilePath] -> IO JSValue
addsToJson = fmap JSArray . mapM addToJson

addsFromJson :: JSValue -> Maybe [(FilePath,  Maybe [String])]
addsFromJson js = takeJsonArray js >>= mapM addFromJson

fullDiffToJson :: Repo -> ([FilePath], [FilePath], [FilePath]) -> IO JSValue
fullDiffToJson repo (r, c, a) = do
      changes    <- changesToJson c
      adds       <- addsToJson a
      info       <- infoToJson repo
      return $ JSObject $ toJSObject [ ("info", info)
                                     , ("removes", removesToJson r)
                                     , ("changes", changes)
                                     , ("adds", adds)]

--readTree :: Repo -> IO JSValue
--readTree = readJsonFromRepo treeFile JSNull
--          
--writeTree :: Repo -> JSValue -> IO () 
--writeTree = writeJsonToRepo treeFile

nameGen :: JSValue -> String
nameGen = T.unpack . encodeHex . hash . B.fromString . Text.JSON.encode

data Diff = Remove { removeIndex :: Int
                   , num :: Int
                   }
          | Add { addIndex :: Int
                , addLines :: [String] 
                }
           deriving(Show,Eq)

--[PolyDiff [(Int,String)] [(Int,String)]]

makeDiff :: [String] -> [String] -> [Diff]
makeDiff = fmap (map conv) 
         . fmap (filter f) 
         . on (D.getGroupedDiffBy $ on (==) snd) enumerate
               where f (D.Both _ _)= False
                     f _ = True
                     conv (D.First a) = Remove (fst $ head a) (length a)
                     conv (D.Second a) = Add (fst $ head a) (map snd a)
                     conv _ = undefined

makeFilePathDiff :: [FilePath] -> [FilePath] -> ([FilePath],[FilePath],[FilePath])
makeFilePathDiff = fmap (foldl fja ([],[],[]))
                 . on D.getDiff sortPaths
                        where fja (x,y,z) (D.First n) = (n : x, y, z)
                              fja (x,y,z) (D.Both n _) = (x, n : y, z)
                              fja (x,y,z) (D.Second n) = (x, y,n : z)                          
                              
genFilePaths :: Repo -> IO [FilePath]
genFilePaths r = do
        ignores <- map ((baseDir r) </>) <$> getIgnores r
        let pom = fileName /=? repoDirName &&? fmap (not . flip elem ignores) filePath 
        find pom pom $ baseDir r
                
                
