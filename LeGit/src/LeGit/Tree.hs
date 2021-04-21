module LeGit.Tree (shaToFP) where

import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as B
import Crypto.Hash.SHA256
import Text.Hex
import System.FilePath
import Text.JSON
import Data.Maybe
import LeGit.Basic

type Sha = String
type Tree = M.HashMap Sha [Sha]

shaGen :: JSValue -> String
shaGen = T.unpack . encodeHex . hash . B.fromString . encode

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
insertNode r shas js = undefined
