module LeGit.Tree (
    getTree, shaToFP, getPredecessors, getPredecessorsShaStr, 
    getPred,
    insertNode, writeTree, getClosestCommonPred) where

import LeGit.Basic

import System.FilePath
import Data.Function
import Data.Maybe
import Text.JSON (encode)
import Data.Text (unpack)
import Text.Hex (encodeHex)
import Crypto.Hash.SHA256 (hash)
import Data.ByteString.UTF8 (fromString)
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S

isAbsolutePureCommit :: PureCommit -> Bool
isAbsolutePureCommit (PureCommit r c a) = any isAbsolute $ r ++ map fst c ++ map fst a

pureCommitToRelative :: Repo -> PureCommit -> PureCommit
pureCommitToRelative repo pc@(PureCommit r c a) = if isAbsolutePureCommit pc 
                                                  then PureCommit (map f r) (zip' c) (zip' a)
                                                  else pc
        where f = (makeRelative $ baseDir repo)
              zip' = mapFst f

commitToRelative :: Repo -> Commit -> Commit
commitToRelative repo (Commit i pc) = Commit i $ pureCommitToRelative repo pc

pureCommitToAbsolute :: Repo -> PureCommit -> PureCommit
pureCommitToAbsolute repo pc@(PureCommit r c a) = if isAbsolutePureCommit pc then pc
                                                  else PureCommit (map f r) (zip' c) (zip' a)
        where f = (baseDir repo </>)
              zip' = mapFst f

commitToAbsolute :: Repo -> Commit -> Commit
commitToAbsolute repo (Commit i pc) = Commit i $ pureCommitToAbsolute repo pc

readCommit :: Repo -> ShaStr -> IO Commit
readCommit r s = commitToAbsolute r
             <$> readJsonFromRepo (`shaToFP` s) (error $ "Internal Error :: Cannot find commit " ++ s) r

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
    | isRoot k m = [k]
    | otherwise  = k : getPredecessorsShaStr' par m
    where par    = head $ getParents k m

getPredecessorsShaStr :: ShaStr -> Tree -> [ShaStr]
getPredecessorsShaStr = fmap reverse . getPredecessorsShaStr'

getPred :: ShaStr -> Repo -> IO Commit
getPred = flip readCommit

shaStrToShaStrs :: Tree -> ShaStr -> [ShaStr]
shaStrToShaStrs = flip getPredecessorsShaStr

getPredecessors :: Repo -> ShaStr -> IO [Commit]
getPredecessors r s = do
    tree <- getTree r
    mapM (`getPred` r) $ shaStrToShaStrs tree s

insertNode :: Repo -> Commit -> [ShaStr] -> IO ShaStr
insertNode r commit parents = do
    let sha' = shaGen commit
    tree <- getTree r
    writeTree r $ M.insert sha' parents tree
    writeJsonToRepo (`shaToFP` sha') r $ commitToRelative r commit
    return sha'

writeTree :: Repo -> Tree -> IO ()
writeTree = writeJsonToRepo treeFile

getClosestCommonPred' :: [ShaStr] -> [ShaStr] -> ShaStr
getClosestCommonPred' s1Pred s2Pred = foldl pom undefined $ zip s1Pred s2Pred
    where pom def (a, b)
            | a == b    = a
            | otherwise = def

getClosestCommonPred :: Tree -> ShaStr -> ShaStr -> Either String ShaStr
getClosestCommonPred t s1 s2
    | not $ M.member s1 t = msg $ s1 ++ " does not exist"
    | not $ M.member s2 t = msg $ s2 ++ " does not exist"
    | s1 == s2            = msg $ s1 ++ " and " ++ s2 ++ " are the same"
    | find' s1 s2         = msg $ s1 ++ " is a predecessor of " ++ s2
    | find' s2 s1         = msg $ s2 ++ " is a predecessor of " ++ s1
    | otherwise           = Right $ getClosestCommonPred' s1Pred s2Pred
        where (s1Pred, s2Pred) = on (,) (flip getPredecessorsShaStr t) s1 s2
              msg              = Left . ("Cannot merge: " ++)
              find' a b        = S.member a $ pom b S.empty
              pom b acc        = foldr pom (S.insert b acc) (fromMaybe undefined $ M.lookup b t)
