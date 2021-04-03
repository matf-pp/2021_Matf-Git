module Lib.Basic (
    --Types and getters
    Repo, baseDir, repoDir,

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

data Repo = Repo {baseDir :: FilePath, repoDir :: FilePath}

repoDirName = ".LeGit"
isRepoDir = fileType ==? Directory &&? fileName ==? repoDirName

fromBaseDir :: FilePath -> Repo
fromBaseDir bd = Repo bd 
               $ joinPath [bd, repoDirName]

fromRepoDir :: FilePath -> Repo
fromRepoDir rd = fromBaseDir $ joinPath $ (reverse . tail . reverse . splitDirectories) rd


listRepos :: FilePath -> IO [Repo]
listRepos fp = fmap fromRepoDir <$> find always isRepoDir fp 

hasRepos :: FilePath -> IO Bool
hasRepos fp = not . null <$> listRepos fp

allParents :: FilePath -> IO [FilePath]
allParents path = reverse . splitDirectories <$> makeAbsolute path >>= return . joinParents
                where joinParents (x:xs) = (joinPath . reverse) (x:xs) : joinParents xs 
                      joinParents x = x

findRepo :: FilePath -> IO (Maybe Repo)
findRepo fp = allParents fp >>= getRepo
                where getRepo (x:xs) = do
                          isRepo <- (\l -> length l == 1) <$> find (depth ==? 0) isRepoDir x
                          if isRepo then return $ Just (fromBaseDir x) else getRepo xs
                      getRepo _ = return Nothing

isRepo :: FilePath -> IO Bool
isRepo fp = not . null <$> findRepo fp