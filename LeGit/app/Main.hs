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
run (Set d args) = directory d >>= flip LeGit.setInfo (map pom args)
    where pom (SetUserName u) = ("username", u)
          pom (SetEmail e) = ("email", e)
run (Print d arg) = directory d >>= pom arg
    where pom PrintUserInfo = LeGit.showInfo
          pom PrintIgnore = LeGit.showIgnores

main :: IO ()
main = execOpt >>= run
