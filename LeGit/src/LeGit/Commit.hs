module LeGit.Commit (nameGen,readFileLines,makeDiff,makeFilePathDiff) where

import Text.JSON
import Crypto.Hash.SHA256
import Text.Hex
import Data.Function
import System.Directory
import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as B
import qualified Data.Algorithm.Diff as D
import qualified Data.HashMap.Strict as M
import Control.Applicative ((<|>))


import LeGit.Basic

removesToJson :: [FilePath] -> JSValue
removesToJson = stringsToJson

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
diffsFromJson js = takeJsonArray js >>= sequenceA . map diffFromJson

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
changesFromJson js = takeJsonArray js >>= sequenceA . map changeFromJson

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
addsToJson = fmap JSArray . sequence . map addToJson

addsFromJson :: JSValue -> Maybe [(FilePath,  Maybe [String])]
addsFromJson js = takeJsonArray js >>= sequenceA . map addFromJson

--addFromJson :: JSValue -> Maybe [()]

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
