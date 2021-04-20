module LeGit.Basic (
    --Types and getters
    Repo, baseDir, repoDir, pointersDir, refsDir, tagsDir, headFile, 
    objectsDir, commitsDir, treeFile, ignoreFile, infoFile,

    --Basic constructors
    fromBaseDir, 

    --Functionality
    listRepos,  --lists all repositories within a directory
    hasRepos,   --check if a repository within the directory exists
    findRepo,   --find the repository this path belongs to by checking if any parent is a repository
    isRepo,     --check if a path belongs to any repository

    --JSON operations
    jsonExt, readJsonFromRepo, writeJsonToRepo,
    stringsToJson, jsonToStrings,
    takeJsonString, takeJsonArray, takeJsonObject, takeJsonInt,

    --FilePath helper functions
    cmpPath, sortPaths, isParent,
    --Utility
    (?), readFileLines, enumerate, getTimeString
) where

import System.FilePath
import System.FilePath.Find
import qualified System.IO.Strict as S
import Text.JSON
import Data.Sort
import Data.Function
import qualified Data.List as L
import qualified Data.HashMap.Strict as M
import Data.Maybe
import Data.Time (getZonedTime)

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

data Repo = Repo {
    baseDir :: FilePath, 
        repoDir :: FilePath,
            pointersDir :: FilePath,
                refsDir :: FilePath,
                tagsDir :: FilePath,
                headFile :: FilePath,
            objectsDir :: FilePath,
                commitsDir :: FilePath,
                treeFile :: FilePath,
                ignoreFile :: FilePath,
            infoFile :: FilePath
}

repoDirName :: String
repoDirName = ".LeGit"

jsonExt :: FilePath -> FilePath
jsonExt = (<.> "json")

isRepoDir :: FindClause Bool
isRepoDir = fileType ==? Directory &&? fileName ==? repoDirName

fromBaseDir :: FilePath -> Repo
fromBaseDir bd = Repo bd (bd </> repoDirName)
                    (joinPath [bd, repoDirName, "pointers"])
                    (joinPath [bd, repoDirName, "pointers", "refs"])
                    (joinPath [bd, repoDirName, "pointers", "tags"])
                    (jsonExt $ joinPath [bd, repoDirName, "pointers", "head"])
                    (joinPath [bd, repoDirName, "objects"])
                    (joinPath [bd, repoDirName, "objects", "commits"])
                    (jsonExt $ joinPath [bd, repoDirName, "objects", "tree"])
                    (jsonExt $ joinPath [bd, repoDirName, "objects", "ignore"])
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

--JSON Stuff

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

readJsonFromRepo :: (Repo -> FilePath) -> JSValue -> Repo -> IO JSValue
readJsonFromRepo f d = fmap (pom . decode) . S.readFile . f
                        where pom (Ok a) = a
                              pom _      = d

writeJsonToRepo :: (Repo -> FilePath) -> Repo -> JSValue -> IO ()
writeJsonToRepo f r = writeFile (f r) . encode

stringsToJson :: [String] -> JSValue
stringsToJson = JSArray . map (JSString . toJSString)

jsonToStrings :: JSValue -> [String]
jsonToStrings = map (fromMaybe undefined) 
              . filter isJust 
              . map takeJsonString 
              . fromMaybe [] 
              . takeJsonArray


--FilePath Stuff

cmpPath :: FilePath -> FilePath -> Ordering
cmpPath = on pom (map dropTrailingPathSeparator . splitPath)
        where pom (x:xs) (y:ys)
                  | x == y = pom xs ys
                  | otherwise = compare x y
              pom a b = on compare null a b        

sortPaths :: [FilePath] -> [FilePath]
sortPaths = sortBy cmpPath

isParent :: FilePath -> FilePath -> Bool
isParent = on L.isPrefixOf (map dropTrailingPathSeparator . splitPath)