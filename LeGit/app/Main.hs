module Main where
import LeGit
import Command

import System.Directory

legit :: Command -> FilePath -> IO ()
legit (Init _ f) d = LeGit.init d f
legit (Commit _ msg) d = LeGit.commit d msg
legit (Set _ args) d = mapM_ pom args
    where pom (SetUserName u)   = LeGit.setUserName d u
          pom (SetEmail e)      = LeGit.setEmail d e
          pom (AddIgnore fp)    = makeAbsolute fp >>= LeGit.addIgnore d
          pom (RemoveIgnore fp) = makeAbsolute fp >>= LeGit.removeIgnore d
          pom (AddRef name)     = LeGit.addRef d name
          pom (AddTag name)     = LeGit.addTag d name
          pom (RemoveRef name)  = LeGit.deleteRef d name
          pom (RemoveTag name)  = LeGit.deleteTag d name
legit (Print _ arg) d = pom arg d
    where pom PrintUserInfo = LeGit.showInfo
          pom PrintIgnore   = LeGit.showIgnores
          pom PrintHead     = LeGit.showHead
          pom PrintStatus   = LeGit.showStatus
          pom PrintTags     = LeGit.showTags
          pom PrintRefs     = LeGit.showRefs
legit (Visit _ vt) d = pom vt
    where pom (VisitRef s)      = LeGit.visitRef d s
          pom (VisitTag s)      = LeGit.visitTag d s
          pom (VisitSha s)      = LeGit.visitSha d s
          pom (VisitRelative n) = LeGit.visitRelative d n
legit (GarbageCollector _) d = LeGit.garbageCollector d
legit (Merge _ name msg) d = LeGit.merge d name msg
legit (Revert _ num msg) d = LeGit.revert d num msg

run :: Command -> IO ()
run c = d >>= legit c
    where d = if null dir
              then getCurrentDirectory
              else makeAbsolute dir
          dir = directory c

main :: IO ()
main = execOpt >>= run
