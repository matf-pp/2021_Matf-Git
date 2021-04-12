module Main where
import LeGit
import Command

import System.Directory

optDirIstance :: String -> IO FilePath
optDirIstance dirStr = if null dirStr
                     then getCurrentDirectory
                     else makeAbsolute dirStr

run :: Command -> IO ()
run Greet = putStrLn "Hi!"
run (Init d f) = optDirIstance d >>= flip LeGit.init f
run (Set d args) = optDirIstance d >>= (\dir -> mapM_ (pom dir) args)
    where pom r (UserName u) = LeGit.setUsername r u
          pom r (Email e) = LeGit.setEmail r e
run (Print d arg) = optDirIstance d >>= pom arg
    where pom UserInfo = LeGit.showInfo

main :: IO ()
main = execOpt >>= run
