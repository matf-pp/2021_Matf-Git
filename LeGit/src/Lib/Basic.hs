module Lib.Basic (
    --Types and getters
    Repo, baseDir, repoDir, infoDir, usernameFile, emailFile,

    --Basic constructors
    fromBaseDir, 

    --Functionality
    listRepos,  --lists all repositories within a directory
    hasRepos,   --check if a repository within the directory exists
    findRepo,   --find the repository this path belongs to by checking if any parent is a repository
    isRepo      --check if a path belongs to any repository
) where

import System.Directory
import System.FilePath
import System.FilePath.Find

data Repo = Repo {
    baseDir :: FilePath, 
    repoDir :: FilePath,
    infoDir :: FilePath,
    usernameFile :: FilePath,
    emailFile :: FilePath
}

repoDirName = ".LeGit"
isRepoDir = fileType ==? Directory &&? fileName ==? repoDirName

fromBaseDir :: FilePath -> Repo
fromBaseDir bd = Repo bd 
                    (joinPath [bd, repoDirName])
                    (joinPath [bd, repoDirName, "info"])
                    (joinPath [bd, repoDirName, "info", "username"])
                    (joinPath [bd, repoDirName, "info", "email"])

fromRepoDir :: FilePath -> Repo
fromRepoDir = fromBaseDir . dropFileName


listRepos :: FilePath -> IO [Repo]
listRepos fp = fmap fromRepoDir <$> find always isRepoDir fp 

hasRepos :: FilePath -> IO Bool
hasRepos = fmap (not . null) . listRepos

findRepo :: FilePath -> IO (Maybe Repo)
findRepo = getRepo . (\(h:t) -> foldl (\acc arg -> (joinPath [head acc, arg]) : acc) [h] t) . splitPath
                where getRepo (x:xs) = not . null <$> find (depth ==? 0) isRepoDir x
                                   >>= (\b -> if b then return . Just $ fromBaseDir x else getRepo xs)
                      getRepo _ = return Nothing

isRepo :: FilePath -> IO Bool
isRepo = fmap (not . null) . findRepo