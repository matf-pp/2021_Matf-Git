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

    --Utility
    (?)
) where

import System.FilePath
import System.FilePath.Find
import qualified System.IO.Strict as S
import Text.JSON

-- Utility functions not based on Repo

infixr 2 ?
(?) :: a -> a -> Bool -> a 
(?) x _ True  = x
(?) _ y False = y

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
listRepos fp = map fromRepoDir <$> find always isRepoDir fp 

hasRepos :: FilePath -> IO Bool
hasRepos = fmap (not . null) . listRepos

findRepo :: FilePath -> IO (Maybe Repo)
findRepo = getRepo . reverse . (scanl1 (</>)) . splitPath
                where getRepo (x:xs) = not . null <$> find (depth ==? 0) isRepoDir x
                                   >>= (return . Just $ fromBaseDir x) ? getRepo xs
                      getRepo _ = return Nothing

isRepo :: FilePath -> IO Bool
isRepo = fmap (not . null) . findRepo


readJsonFromRepo :: (Repo -> FilePath) -> JSValue -> Repo -> IO JSValue
readJsonFromRepo f d = fmap (pom . decode) . S.readFile . f
                        where pom (Ok a) = a
                              pom _      = d

writeJsonToRepo :: (Repo -> FilePath) -> Repo -> JSValue -> IO ()
writeJsonToRepo f r = writeFile (f r) . encode