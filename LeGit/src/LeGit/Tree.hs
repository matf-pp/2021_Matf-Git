module LeGit.Tree () where

import LeGit.Basic

import System.FilePath
import Data.Maybe
import Text.JSON (encode, JSValue)
import Data.Text (unpack)
import Text.Hex (encodeHex)
import Crypto.Hash.SHA256 (hash)
import Data.ByteString.UTF8 (fromString)
import qualified Data.HashMap.Strict as M

type Sha = String
type Tree = M.HashMap Sha [Sha]

shaGen :: JSValue -> String
shaGen = unpack . encodeHex . hash . fromString . encode

shaToFP :: Repo -> Sha -> FilePath
shaToFP r s = jsonExt $ (commitsDir r) </> s

shaFromFP :: FilePath -> Sha
shaFromFP = takeBaseName

getParents :: Sha -> Tree -> [Sha]
getParents k m = fromMaybe undefined (M.lookup k m)

isRoot :: Sha -> Tree -> Bool
isRoot k m = null (getParents k m)

getPredecessors :: Sha -> Tree -> [Sha]
getPredecessors k m
    | isRoot k m = []
    | otherwise  = getPredecessors par m ++ [par]
    where par    = getParents k m !! 0

insertNode :: Repo -> [Sha] -> JSValue -> IO ()
insertNode = undefined
