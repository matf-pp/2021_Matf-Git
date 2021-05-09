module LeGit.Action (
    LeGit.Action.commit,
    visitRef, visitTag, visitSha,
    garbageCollector
) where

import LeGit.Basic
import LeGit.Pointers
import LeGit.Commit
import LeGit.GarbageCollector

actionErrorCheck :: (Repo -> IO ()) -> FilePath -> IO ()
actionErrorCheck f fp = findRepo fp >>= pom
    where pom (Just r) = f r
          pom Nothing  = errorMsg $ "action on " ++ fp ++ " can't be done: not a repository"

commit :: FilePath -> String -> IO ()
commit fp msg = actionErrorCheck pom fp
    where pom r = do
            LeGit.Commit.commit r msg
            currHead <- phead <$> getPointers r
            putStrLn $ "HEAD -> " ++ show currHead

visitRef :: FilePath -> String -> IO ()
visitRef fp s = actionErrorCheck pom fp
            where pom r = do
                    setHeadFromRef r s
                    visit r
                    putStrLn $ "HEAD -> " ++ s
                  
visitTag :: FilePath -> String -> IO ()
visitTag fp s = actionErrorCheck pom fp
            where pom r = do
                    setHeadFromTag r s
                    visit r
                    putStrLn $ "HEAD -> " ++ s

visitSha :: FilePath -> String -> IO ()
visitSha fp s = actionErrorCheck pom fp
            where pom r = do
                    setHeadFromSha r s
                    visit r
                    currSha <- sha . phead <$> getPointers r
                    putStrLn $ "HEAD -> " ++ currSha

garbageCollector :: FilePath -> IO ()
garbageCollector = actionErrorCheck gc
