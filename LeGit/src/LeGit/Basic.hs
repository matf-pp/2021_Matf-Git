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
import qualified System.IO.Strict as S
import qualified Data.List as L

import LeGit.Types

-- Utility functions not based on Repo


readFileLines :: FilePath -> IO [String]
readFileLines = fmap lines . S.readFile 

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

getTimeString :: IO String
getTimeString = takeWhile (/= '.') . show <$> getZonedTime

-- Repo stuff

fromRepoDir :: FilePath -> Repo
fromRepoDir = fromBaseDir . dropFileName

readJsonFromRepo :: JSON a => (Repo -> FilePath) -> a -> Repo -> IO a
readJsonFromRepo f d r = do
    let fp = f r
        pom (Ok a)    = a
        pom (Error s) = error $ "Internal Error for " ++ fp ++ " ::\n" ++ s
    b <- doesFileExist fp
    if b then pom . decode <$> S.readFile fp else return d

writeJsonToRepo :: JSON a => (Repo -> FilePath) -> Repo -> a -> IO ()
writeJsonToRepo f r = writeFile (f r) . encode

isRepoDir :: FindClause Bool
isRepoDir = fileType ==? Directory &&? fileName ==? repoDirName


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
