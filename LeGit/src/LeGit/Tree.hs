module LeGit.Tree (nameGen) where

import Text.JSON
import Crypto.Hash.SHA256
import Text.Hex
import Data.Text
import Data.ByteString.UTF8
import LeGit.Basic

readTree :: Repo -> IO JSValue
readTree = readJsonFromRepo treeFile JSNull
          
writeTree :: Repo -> JSValue -> IO () 
writeTree = writeJsonToRepo treeFile

nameGen :: JSValue -> String
nameGen = unpack . encodeHex . hash . fromString . Text.JSON.encode

