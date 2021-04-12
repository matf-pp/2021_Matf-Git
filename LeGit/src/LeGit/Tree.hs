module LeGit.Tree () where

import Text.JSON
import Crypto.Hash.SHA256
import Text.Hex
import Data.Text
import Data.ByteString.UTF8
import LeGit.Basic

readTree :: Repo -> IO JSValue
readTree r = pom . Text.JSON.decode <$> readFile (treeFile r)
    where pom (Ok rez) = rez
          pom _ = JSNull
          
writeTree :: Repo -> JSValue -> IO () 
writeTree r js = writeFile (treeFile r) (Text.JSON.encode js)


nameGen :: JSValue -> String
nameGen = unpack . encodeHex . hash . fromString . Text.JSON.encode

