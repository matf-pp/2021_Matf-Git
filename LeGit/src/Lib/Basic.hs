module Lib.Basic (
    Repo,
    repoDirName,
    hasRepos,
    findRepo,
    isRepo
) where

import System.Directory
import System.FilePath
import System.FilePath.Find

data Repo = Repo {baseDir :: FilePath, repoDir :: FilePath}

repoDirName = ".LeGit"
isRepoDir = fileType ==? Directory &&? fileName ==? repoDirName

hasRepos :: FilePath -> IO Bool
hasRepos fp = not . null <$> listRepos fp
              where listRepos = find always isRepoDir

allParents :: FilePath -> IO [FilePath]
allParents path = reverse . splitDirectories <$> makeAbsolute path >>= return . joinParents
                where joinParents (x:xs) = (joinPath . reverse) (x:xs) : joinParents xs 
                      joinParents x = x

findRepo :: FilePath -> IO (Maybe Repo)
findRepo fp = allParents fp >>= getRepo
                where getRepo (x:xs) = do
                          isRepo <- (\l -> length l == 1) <$> find (depth ==? 0) isRepoDir x
                          if isRepo then return $ Just (fromBaseDir x) else getRepo xs
                                where fromBaseDir x = Repo fp $ joinPath [x, repoDirName]
                      getRepo _ = return Nothing

isRepo :: FilePath -> IO Bool
isRepo fp = not . null <$> findRepo fp