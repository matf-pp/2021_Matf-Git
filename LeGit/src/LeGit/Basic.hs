module LeGit.Basic (
    --Types and getters
    Repo, baseDir, repoDir, pointersFile, infoFile, repoDirName,
    objectsDir, commitsDir, treeFile, ignoreFile, fromBaseDir, 
    Diff(Add,Remove),
    Contents(File,Dir),
    Commit, commitInfo, commitRemoves, commitAdds, commitChanges,
    Head(Ref,Tag,Sha),
    Pointers, phead, refs, tags,
    

    --Functionality
    listRepos,  --lists all repositories within a directory
    hasRepos,   --check if a repository within the directory exists
    findRepo,   --find the repository this path belongs to by checking if any parent is a repository
    isRepo,     --check if a path belongs to any repository
    readJsonFromRepo, writeJsonToRepo,

    --FilePath helper functions
    cmpPath, sortPaths, isParent, jsonExt,

    --Utility
    (?), readFileLines, enumerate, getTimeString
) where

import System.FilePath
import System.Directory
import System.FilePath.Find
import Data.Sort
import Data.Function
import Text.JSON
import Data.Time (getZonedTime)
import Control.Applicative ((<|>))
import qualified System.IO.Strict as S
import qualified Data.List as L
import qualified Data.HashMap.Strict as M
import qualified Data.Hashable as H


-- Types

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

-- Utility functions not based on Repo

infixr 2 ?
(?) :: a -> a -> Bool -> a 
(?) x _ True  = x
(?) _ y False = y

readFileLines :: FilePath -> IO [String]
readFileLines = fmap lines . S.readFile 

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

getTimeString :: IO String
getTimeString = takeWhile (/= '.') . show <$> getZonedTime

-- Repo stuff

readJsonFromRepo :: JSON a => (Repo -> FilePath) -> a -> Repo -> IO a
readJsonFromRepo f d r = do
    let fp = f r
        pom (Ok a)    = a
        pom (Error s) = error $ "Internal Error for " ++ fp ++ " ::\n" ++ s
    b <- doesFileExist fp
    if b then pom . decode <$> S.readFile fp else return d

writeJsonToRepo :: JSON a => (Repo -> FilePath) -> Repo -> a -> IO ()
writeJsonToRepo f r = writeFile (f r) . encode

repoDirName :: String
repoDirName = ".LeGit"

objectsDirName :: String
objectsDirName = "objects"

isRepoDir :: FindClause Bool
isRepoDir = fileType ==? Directory &&? fileName ==? repoDirName

fromBaseDir :: FilePath -> Repo
fromBaseDir bd = Repo bd (bd </> repoDirName)
                    (joinPath [bd, repoDirName, objectsDirName])
                    (joinPath [bd, repoDirName, objectsDirName, "commits"])
                    (jsonExt $ joinPath [bd, repoDirName, objectsDirName, "tree"])
                    (jsonExt $ joinPath [bd, repoDirName, objectsDirName, "ignore"])
                    (jsonExt $ joinPath [bd, repoDirName, objectsDirName, "pointers"])
                    (jsonExt $ joinPath [bd, repoDirName, "info"])

fromRepoDir :: FilePath -> Repo
fromRepoDir = fromBaseDir . dropFileName


listRepos :: FilePath -> IO [Repo]
listRepos fp = map fromRepoDir <$> find (fileName /=? repoDirName) isRepoDir fp 

hasRepos :: FilePath -> IO Bool
hasRepos = fmap (not . null) . listRepos

findRepo :: FilePath -> IO (Maybe Repo)
findRepo = getRepo . reverse . (scanl1 (</>)) . splitPath
                where getRepo (x:xs) = not . null <$> find (depth ==? 0) isRepoDir x
                                   >>= (return . Just $ fromBaseDir x) ? getRepo xs
                      getRepo _ = return Nothing

isRepo :: FilePath -> IO Bool
isRepo = fmap (not . null) . findRepo

--FilePath Stuff

cmpPath :: FilePath -> FilePath -> Ordering
cmpPath = on pom (map dropTrailingPathSeparator . splitPath)
        where pom (x:xs) (y:ys)
                  | x == y = pom xs ys
                  | otherwise = compare x y
              pom a b = on compare null b a       

sortPaths :: [FilePath] -> [FilePath]
sortPaths = sortBy cmpPath

isParent :: FilePath -> FilePath -> Bool
isParent = on L.isPrefixOf (map dropTrailingPathSeparator . splitPath)

jsonExt :: FilePath -> FilePath
jsonExt = (<.> "json")
