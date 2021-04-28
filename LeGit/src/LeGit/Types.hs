module LeGit.Types (
    Diff(Add,Remove),
    Contents(File,Dir),
    DirStruct,
    Commit(Commit), commitInfo, commitRemoves, commitAdds, commitChanges,
    ShaStr,
    Head(Ref,Tag,Sha),
    Pointers(Pointers), phead, refs, tags
) where

import Text.JSON
import qualified Data.HashMap.Strict as M
import qualified Data.Hashable as H
import Control.Applicative ((<|>))

rerror :: String -> Result a
rerror t = Error ("Failed to parse Json Object type " ++ t)

construct :: JSON a => String -> (JSObject JSValue -> Result a) -> JSObject JSValue -> Result a
construct t f m = do
    b <- (== t) <$> valFromObj "type" m
    if b then f m else rerror t


instance (Eq k, H.Hashable k, JSKey k, JSON v) => JSON (M.HashMap k v) where
    showJSON = encJSDict . M.toList
    readJSON = fmap M.fromList . decJSDict "Failed to parse Json Object as HashMap"

data Diff = Remove { 
    removeIndex :: Int, 
    num :: Int
}         | Add { 
    addIndex :: Int, 
    addLines :: [String] 
}
    deriving(Show,Eq)

instance JSON Diff where
    showJSON (Remove i n) = makeObj [("type", showJSON "remove")
                                    ,("index", showJSON i)
                                    ,("num", showJSON n)]
    showJSON (Add i ls)   = makeObj [("type", showJSON "add")
                                    ,("index", showJSON i)
                                    ,("lines", showJSONs ls)]
    readJSON js = readJSON js >>= getDiff
        where getDiff m = getAdd m <|> getRemove m <|> rerror "Diff"
              getAdd    = construct "add" add'
              add' m    = Add <$> valFromObj "index" m <*> valFromObj "lines" m
              getRemove = construct "remove" remove'
              remove' m = Remove <$> valFromObj "index" m <*> valFromObj "num" m 

data Contents = File [String] | Dir
    deriving Show 

instance JSON Contents where
    showJSON (File a) = showJSONs a
    showJSON Dir      = JSNull
    readJSON js = getFile js <|> getDir js
        where getDir v = if v == JSNull then return Dir
                         else rerror "Contents"
              getFile  = fmap File . readJSON
              
type DirStruct = M.HashMap FilePath Contents

data Commit = Commit { 
    commitInfo    :: M.HashMap String String, 
    commitRemoves :: [FilePath],
    commitChanges :: [(FilePath, [Diff])],
    commitAdds    :: [(FilePath, Contents)]
}

instance JSON Commit where
    showJSON (Commit i r c a) = makeObj [("info", showJSON i)
                                        ,("removes", showJSON r)
                                        ,("changes", showJSON c)
                                        ,("adds", showJSON a)]
    readJSON js = readJSON js >>= getCommit
        where getCommit m = Commit 
                        <$> valFromObj "info" m
                        <*> valFromObj "removes" m
                        <*> valFromObj "changes" m
                        <*> valFromObj "adds" m

type ShaStr = String

data Head = Ref { ref :: String }
          | Tag { tag :: String }
          | Sha { sha :: ShaStr }
    deriving(Show,Eq)

instance JSON Head where
    showJSON (Ref p) = encJSDict [("type", "ref"), ("value", p)]
    showJSON (Tag p) = encJSDict [("type", "tag"), ("value", p)]
    showJSON (Sha p) = encJSDict [("type", "sha"), ("value", p)]
    readJSON js = readJSON js >>= getHead
        where getHead m = getRef m <|> getTag m <|> getSha m <|> rerror "Head"
              getRef    = construct "ref" ref'
              ref' m    = Ref <$> valFromObj "value" m
              getTag    = construct "tag" tag'
              tag' m    = Tag <$> valFromObj "value" m
              getSha    = construct "sha" sha'
              sha' m    = Sha <$> valFromObj "value" m

data Pointers = Pointers {
    phead :: Head,
    refs  :: M.HashMap String ShaStr,
    tags  :: M.HashMap String ShaStr
}

instance JSON Pointers where
    showJSON (Pointers h r t) = makeObj [("head", showJSON h)
                                        ,("refs", showJSON r)
                                        ,("tags", showJSON t)]
    readJSON js = readJSON js >>= getPointers
        where getPointers m = Pointers
                          <$> valFromObj "head" m
                          <*> valFromObj "refs" m
                          <*> valFromObj "tags" m
