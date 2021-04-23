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
shaToFP r = jsonExt . (commitsDir r </>)

shaFromFP :: FilePath -> Sha
shaFromFP = takeBaseName

getParents :: Sha -> Tree -> [Sha]
getParents = fmap (fromMaybe undefined) . M.lookup

isRoot :: Sha -> Tree -> Bool
isRoot = fmap null . getParents

getPredecessors :: Sha -> Tree -> [Sha]
getPredecessors k m
    | isRoot k m = []
    | otherwise  = getPredecessors par m ++ [par]
    where par    = head $ getParents k m

insertNode :: Repo -> [Sha] -> JSValue -> IO ()
insertNode = undefined
