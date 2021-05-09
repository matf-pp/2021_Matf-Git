module LeGit.GarbageCollector (gc) where

import LeGit.Basic
import LeGit.Pointers
import LeGit.Tree
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import Data.Maybe
import Data.Function
import System.Directory

gc' :: Pointers -> Tree -> [ShaStr]
gc' (Pointers _ r t) tree = S.toList $ foldr pom everySha visibleShas
    where everySha        = S.fromList $ M.keys tree
          visibleShas     = on (++) M.elems r t
          pom s acc       = if S.member s acc then foldr pom (S.delete s acc) (fromMaybe undefined $ M.lookup s tree)
                                              else acc

gc :: Repo -> IO ()
gc r = do
    p@(Pointers h _ _) <- getPointers r
    tree <- getTree r
    if isSha h then errorMsg "Cannot start garbage collector when Head is Sha"
               else return ()
    let blacklist = gc' p tree
    writeTree r $ foldr M.delete tree blacklist
    mapM_ removeFile $ map (shaToFP r) blacklist
