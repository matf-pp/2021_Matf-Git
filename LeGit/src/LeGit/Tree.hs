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

type Tree = M.HashMap ShaStr [ShaStr]

getTree :: Repo -> IO Tree
getTree = readJsonFromRepo treeFile M.empty

shaGen :: Commit -> ShaStr
shaGen = unpack . encodeHex . hash . fromString . encode

shaToFP :: Repo -> ShaStr -> FilePath
shaToFP r = jsonExt . (commitsDir r </>)

--shaFromFP :: FilePath -> ShaStr
--shaFromFP = takeBaseName

getParents :: ShaStr -> Tree -> [ShaStr]
getParents s = fromMaybe e . M.lookup s
    where e = error $ "Internal Error :: Cannot find commit " ++ s

isRoot :: ShaStr -> Tree -> Bool
isRoot = fmap null . getParents

getPredecessorsShaStr' :: ShaStr -> Tree -> [ShaStr]
getPredecessorsShaStr' k m
    | isRoot k m = []
    | otherwise  = par : getPredecessorsShaStr' par m
    where par    = head $ getParents k m

getPredecessorsShaStr :: ShaStr -> Tree -> [ShaStr]
getPredecessorsShaStr = fmap reverse . getPredecessorsShaStr'

getPred :: ShaStr -> Repo -> IO Commit
getPred s = readJsonFromRepo (`shaToFP` s) (error $ "Internal Error :: Cannot find commit " ++ s)

shaStrToShaStrs :: Tree -> ShaStr -> [ShaStr]
shaStrToShaStrs = flip getPredecessorsShaStr

getPredecessors :: Repo -> ShaStr -> IO [Commit]
getPredecessors r s = do
    tree <- getTree r
    mapM (`getPred` r) $ shaStrToShaStrs tree s

insertNode :: Repo -> Commit -> [ShaStr] -> IO ShaStr
insertNode r commit parents = do
    let sha = shaGen commit
    tree <- getTree r
    writeJsonToRepo treeFile r $ M.insert sha parents tree
    writeJsonToRepo (`shaToFP` sha) r commit
    return sha

