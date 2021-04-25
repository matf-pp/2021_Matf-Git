module LeGit.Types (
    Repo, baseDir, repoDir, pointersFile, infoFile, repoDirName,
    objectsDir, commitsDir, treeFile, ignoreFile, fromBaseDir, 
    Diff(Add,Remove),
    Contents(File,Dir),
    DirStruct,
    Commit, commitInfo, commitRemoves, commitAdds, commitChanges,
    Head(Ref,Tag,Sha),
    Pointers, phead, refs, tags,
    jsonExt,
    (?) -- i like this operator very much
) where

import Text.JSON
import System.FilePath
import qualified Data.HashMap.Strict as M
import qualified Data.Hashable as H
import Control.Applicative ((<|>))

infixr 2 ?
(?) :: a -> a -> Bool -> a 
(?) x _ True  = x
(?) _ y False = y

repoDirName :: String
repoDirName = ".LeGit"

objectsDirName :: String
objectsDirName = "objects"

jsonExt :: FilePath -> FilePath
jsonExt = (<.> "json")

fromBaseDir :: FilePath -> Repo
fromBaseDir bd = Repo bd (bd </> repoDirName)
                    (joinPath [bd, repoDirName, objectsDirName])
                    (joinPath [bd, repoDirName, objectsDirName, "commits"])
                    (jsonExt $ joinPath [bd, repoDirName, objectsDirName, "tree"])
                    (jsonExt $ joinPath [bd, repoDirName, objectsDirName, "ignore"])
                    (jsonExt $ joinPath [bd, repoDirName, objectsDirName, "pointers"])
                    (jsonExt $ joinPath [bd, repoDirName, "info"])

instance (Eq k, H.Hashable k, JSKey k, JSON v) => JSON (M.HashMap k v) where
    showJSON = encJSDict . M.toList
    readJSON = fmap M.fromList . decJSDict "Not a HashMap"

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
        where getDiff m   = getAdd m <|> getRemove m
              getType     = valFromObj "type"
              getIndex    = valFromObj "index"
              isAdd       = fmap (== "add") . getType
              isRemove    = fmap (== "remove") . getType
              getAdd m    = isAdd m 
                        >>= Add <$> getIndex m <*> valFromObj "lines" m 
                          ? Error "Not a Diff"
              getRemove m = isRemove m 
                        >>= Remove <$> getIndex m <*> valFromObj "num" m 
                          ? Error "Not a Diff"


data Contents = File [String] | Dir
    deriving Show

instance JSON Contents where
    showJSON (File a) = showJSONs a
    showJSON Dir      = JSNull
    readJSON js = getDir js <|> getFile js
        where getDir v = if (v == JSNull) 
                         then return Dir
                         else Error "Not a Contents"
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

data Head = Ref { getRef :: String }
          | Tag { getTag :: String }
          | Sha { getSha :: String }
    deriving(Show,Eq)

instance JSON Head where
    showJSON (Ref p) = encJSDict [("type", "ref"), ("value", p)]
    showJSON (Tag p) = encJSDict [("type", "tag"), ("value", p)]
    showJSON (Sha p) = encJSDict [("type", "sha"), ("value", p)]
    readJSON js = readJSON js >>= getHead
        where getHead m = getRef m <|> getTag m <|> getSha m
              getType   = valFromObj "type"
              getVal    = valFromObj "value"
              getRef m  = fmap (== "ref") (getType m) 
                      >>= Ref <$> getVal m ? Error "Not a Head"
              getTag m  = fmap (== "tag") (getType m) 
                      >>= Tag <$> getVal m ? Error "Not a Head"
              getSha m  = fmap (== "Sha") (getType m) 
                      >>= Sha <$> getVal m ? Error "Not a Head"

data Pointers = Pointers {
    phead :: Head,
    refs  :: M.HashMap String String,
    tags  :: M.HashMap String String
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

data Repo = Repo {
    baseDir :: FilePath, 
        repoDir :: FilePath,
            objectsDir :: FilePath,
                commitsDir :: FilePath,
                treeFile :: FilePath,
                ignoreFile :: FilePath,
                pointersFile :: FilePath,
            infoFile :: FilePath
}
