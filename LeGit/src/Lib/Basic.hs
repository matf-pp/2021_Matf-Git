module Lib.Basic (
    --Types and getters
    Repo, baseDir, repoDir, infoDir, usernameFile, emailFile, 
        pointersDir, refsDir, tagsDir, headFile, objectsDir, commitsDir, treeFile, ignoreFile,

    --Basic constructors
    fromBaseDir, 

    --Functionality
    listRepos,  --lists all repositories within a directory
    hasRepos,   --check if a repository within the directory exists
    findRepo,   --find the repository this path belongs to by checking if any parent is a repository
    isRepo,     --check if a path belongs to any repository

    --Utility
    (?)
) where

import System.FilePath
import System.FilePath.Find

-- Utility functions not based on Repo

infixr 2 ?
(?) :: a -> a -> Bool -> a 
(?) x _ True  = x
(?) _ y False = y

-- Repo stuff

data Repo = Repo {
    baseDir :: FilePath, 
        repoDir :: FilePath,
            infoDir :: FilePath,
                usernameFile :: FilePath,
                emailFile :: FilePath,
            pointersDir :: FilePath,
                refsDir :: FilePath,
                tagsDir :: FilePath,
                headFile :: FilePath,
            objectsDir :: FilePath,
                commitsDir :: FilePath,
                treeFile :: FilePath,
                ignoreFile :: FilePath
}

repoDirName :: String
repoDirName = ".LeGit"

isRepoDir :: FindClause Bool
isRepoDir = fileType ==? Directory &&? fileName ==? repoDirName

fromBaseDir :: FilePath -> Repo
fromBaseDir bd = Repo bd (bd </> repoDirName)
                    (joinPath [bd, repoDirName, "info"])
                    (joinPath [bd, repoDirName, "info", "username"])
                    (joinPath [bd, repoDirName, "info", "email"])
                    (joinPath [bd, repoDirName, "pointers"])
                    (joinPath [bd, repoDirName, "pointers", "refs"])
                    (joinPath [bd, repoDirName, "pointers", "tags"])
                    (joinPath [bd, repoDirName, "pointers", "head"])
                    (joinPath [bd, repoDirName, "objects"])
                    (joinPath [bd, repoDirName, "objects", "commits"])
                    (joinPath [bd, repoDirName, "objects", "tree.json"])
                    (joinPath [bd, repoDirName, "objects", "ignore"])

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
