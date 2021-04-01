module Lib.Init (Lib.Init.init) where

init :: String -> Bool -> IO ()
init d f = putStrLn $ "Initializing " ++ d ++ if f then " with FORCE" else ""