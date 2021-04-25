module LeGit.Tree (getPredecessors, insertNode) where

import LeGit.Basic

import System.FilePath
import Data.Maybe
import Text.JSON (encode)
import Data.Text (unpack)
import Text.Hex (encodeHex)
import Crypto.Hash.SHA256 (hash)
import Data.ByteString.UTF8 (fromString)
import qualified Data.HashMap.Strict as M

type Sha = String
type Tree = M.HashMap Sha [Sha]

getTree :: Repo -> IO Tree
getTree = readJsonFromRepo treeFile M.empty

shaGen :: Commit -> Sha
shaGen = unpack . encodeHex . hash . fromString . encode

shaToFP :: Repo -> Sha -> FilePath
shaToFP r = jsonExt . (commitsDir r </>)

shaFromFP :: FilePath -> Sha
shaFromFP = takeBaseName

getParents :: Sha -> Tree -> [Sha]
getParents = fmap (fromMaybe undefined) . M.lookup

isRoot :: Sha -> Tree -> Bool
isRoot = fmap null . getParents

getPredecessorsSha' :: Sha -> Tree -> [Sha]
getPredecessorsSha' k m
    | isRoot k m = []
    | otherwise  = par : getPredecessorsSha' par m
    where par    = head $ getParents k m

getPredecessorsSha :: Sha -> Tree -> [Sha]
getPredecessorsSha = fmap reverse . getPredecessorsSha'

getPred :: Sha -> Repo -> IO Commit
getPred s = readJsonFromRepo (flip shaToFP s) (error $ "Internal Error :: Cannot find commit " ++ s)

shaToShas :: Tree -> Sha -> [Sha]
shaToShas t = flip getPredecessorsSha t

getPredecessors :: Repo -> Sha -> IO [Commit]
getPredecessors r s = do
    tree <- getTree r
    mapM (flip getPred r) $ shaToShas tree s

insertNode :: Repo -> Commit -> [Sha] -> IO ()
insertNode r commit parents = do
    let sha = shaGen commit
    tree <- getTree r
    writeJsonToRepo treeFile r $ M.insert sha parents tree
    writeJsonToRepo (flip shaToFP sha) r commit