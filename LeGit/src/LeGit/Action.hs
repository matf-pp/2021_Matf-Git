module LeGit.Action (
    LeGit.Action.commit, LeGit.Action.merge,
    visitRef, visitTag, visitSha, visitRelative,
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

merge :: FilePath -> String -> String -> IO ()
merge fp name msg = actionErrorCheck f fp
    where f r = do
            oldHead <- phead <$> getPointers r
            LeGit.Commit.merge r name msg
            let (Ref old) = oldHead
            putStrLn $ old ++ " + " ++ name ++ " -> " ++ name
            putStrLn $ "HEAD -> " ++ name

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

visitRelative :: FilePath -> Int -> IO ()
visitRelative fp i = actionErrorCheck pom fp
            where pom r = do
                    setHeadRelative r i
                    visit r
                    currSha <- sha . phead <$> getPointers r
                    putStrLn $ "HEAD -> " ++ currSha

garbageCollector :: FilePath -> IO ()
garbageCollector = actionErrorCheck gc
