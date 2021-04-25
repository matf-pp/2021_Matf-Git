module LeGit.Basic (
    --Types and getters
    Repo, baseDir, repoDir, pointersFile, infoFile, repoDirName,
    objectsDir, commitsDir, treeFile, ignoreFile,
    Diff(Add,Remove),
    Head(Ref,Tag,Sha),

    --Basic constructors
    fromBaseDir, 

    --Functionality
    listRepos,  --lists all repositories within a directory
    hasRepos,   --check if a repository within the directory exists
    findRepo,   --find the repository this path belongs to by checking if any parent is a repository
    isRepo,     --check if a path belongs to any repository

    --FilePath helper functions
    cmpPath, sortPaths, isParent, jsonExt,

    --Utility
    (?), readFileLines, enumerate, getTimeString
) where

import System.FilePath
import System.FilePath.Find
import Data.Sort
import Data.Function
import Text.JSON
import Data.Time (getZonedTime)
import qualified System.IO.Strict as S
import qualified Data.List as L

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

data Diff = Remove { 
    removeIndex :: Int, 
    num :: Int
}         | Add { 
    addIndex :: Int, 
    addLines :: [String] 
}
    deriving(Show,Eq)

data Head = Ref { getRef :: String }
          | Tag { getTag :: String }
          | Sha { getSha :: String }
    deriving(Show,Eq)

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
