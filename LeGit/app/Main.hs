module Main where
import LeGit
import Command

import System.Directory

directory :: String -> IO FilePath
directory dirStr = if null dirStr
                     then getCurrentDirectory
                     else makeAbsolute dirStr

run :: Command -> IO ()
run Greet = putStrLn "Hi!"
run (Init d f) = directory d >>= flip LeGit.init f
run (Set d args) = directory d >>= flip mapM_ args . pom
    where pom r (SetUserName u) = LeGit.setUserName r u
          pom r (SetEmail e) = LeGit.setEmail r e
          pom r (AddIgnore fp) = makeAbsolute fp >>= LeGit.addIgnore r
          pom r (RemoveIgnore fp) = makeAbsolute fp >>= LeGit.removeIgnore r
run (Print d arg) = directory d >>= pom arg
    where pom PrintUserInfo = LeGit.showInfo
          pom PrintIgnore = LeGit.showIgnores
run (Commit d msg) = directory d >>= flip LeGit.commit msg

main :: IO ()
main = execOpt >>= run
