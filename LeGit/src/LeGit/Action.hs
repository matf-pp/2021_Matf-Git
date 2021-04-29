module LeGit.Action (LeGit.Action.commit) where

import LeGit.Basic
import LeGit.Commit (commit)

actionErrorCheck :: (Repo -> IO ()) -> FilePath -> IO ()
actionErrorCheck f fp = findRepo fp >>= pom
    where pom (Just r) = f r
          pom Nothing  = errorMsg $ "action on " ++ fp ++ " can't be done: not a repository"

commit :: FilePath -> IO ()
commit = actionErrorCheck LeGit.Commit.commit