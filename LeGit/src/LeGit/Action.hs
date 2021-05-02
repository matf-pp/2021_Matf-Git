module LeGit.Action (
    LeGit.Action.commit,
    visitRef, visitTag, visitSha
) where

import LeGit.Basic
import LeGit.Pointers
import LeGit.Commit

actionErrorCheck :: (Repo -> IO ()) -> FilePath -> IO ()
actionErrorCheck f fp = findRepo fp >>= pom
    where pom (Just r) = f r
          pom Nothing  = errorMsg $ "action on " ++ fp ++ " can't be done: not a repository"

commit :: FilePath -> IO ()
commit = actionErrorCheck LeGit.Commit.commit

visitRef :: FilePath -> String -> IO ()
visitRef fp s = actionErrorCheck pom fp
            where pom r = do
                    setHeadFromRef r s
                    visit r
                  
visitTag :: FilePath -> String -> IO ()
visitTag fp s = actionErrorCheck pom fp
            where pom r = do
                    setHeadFromTag r s
                    visit r

visitSha :: FilePath -> String -> IO ()
visitSha fp s = actionErrorCheck pom fp
            where pom r = do
                    setHeadFromSha r s
                    visit r