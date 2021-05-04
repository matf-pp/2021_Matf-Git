{-# LANGUAGE TupleSections #-}
module LeGit.Commit (commit,makeDiff,makeFilePathDiff,visit) where

import LeGit.Basic
import LeGit.Info
import LeGit.Ignore (getIgnores)
import LeGit.Pointers

import Data.Function
import Data.Maybe
import System.FilePath
import System.FilePath.Find
import qualified Data.Algorithm.Diff as D
import qualified Data.HashMap.Strict as M
import System.Directory
import Data.Sort

makeCommitInfo :: Repo -> String -> IO (M.HashMap String String)
makeCommitInfo r msg = do
      u <- (,) "username" <$> getUserNameAssert r
      t <- (,) "time" <$> getTimeString
      e <- map ("email",) . catMaybes . pure <$> getInfo "email" r
      let m = (,) "message" msg
      return $ M.fromList $ u : t : m : e     

makeDiff :: [String] -> [String] -> [Diff]
makeDiff = fmap (map conv . filter f) . on (D.getGroupedDiffBy $ on (==) snd) enumerate
               where f (D.Both _ _)= False
                     f _ = True
                     conv (D.First a) = Remove (fst $ head a) (length a)
                     conv (D.Second a) = Add (fst $ head a) (map snd a)
                     conv _ = undefined

makeFilePathDiff :: [FilePath] -> [FilePath] -> ([FilePath],[FilePath],[FilePath])
makeFilePathDiff = fmap (foldl fja ([],[],[]))
                 . on D.getDiff sortPaths
                        where fja (x,y,z) (D.First n) = (x, y,n : z)
                              fja (x,y,z) (D.Both n _) = (x, n : y, z)
                              fja (x,y,z) (D.Second n) = (n : x, y, z)

genFilePaths :: Repo -> IO [FilePath]
genFilePaths r = do
        ignores <- map (baseDir r </>) <$> getIgnores r
        let pom = fileName /=? repoDirName &&? fmap (not . flip elem ignores) filePath
        find pom (pom &&? filePath /=? baseDir r) $ baseDir r

remove :: DirStruct -> Commit -> DirStruct
remove acc = foldl remove' acc . commitRemoves
        where remove' acc' fp = M.delete fp acc'

add :: DirStruct -> Commit -> DirStruct
add acc = foldl add' acc . commitAdds
        where add' acc' (k,v) = M.insert k v acc'

change :: DirStruct -> Commit -> DirStruct
change acc = foldl (flip change') acc . commitChanges
        where change' (fp,difs) = M.insert fp $ File $ flip pom difs $ getOld fp acc
              getOld = fmap (fromMaybe undefined) . M.lookup
              pom (File old) = fst . foldl pom' (old,0)
              pom Dir = undefined
              pom' (old,off) (Remove ind br) = (take (ind-1 + off) old ++ drop (ind -1 + br + off) old,off - br)
              pom' (old,off) (Add ind s) = (insertBetween s $ flip splitAt old $ ind-1,off + length s)
              insertBetween s (l,r) = l ++ s ++ r


reconstruct :: [Commit] -> DirStruct
reconstruct = foldl (flip pom) M.empty
        where pom com = flip change com . flip add com . flip remove com

makeRemoveList :: [FilePath] -> [FilePath]
makeRemoveList = reverse . sortPaths

makeAddList :: [FilePath] -> IO [(FilePath,Contents)]
makeAddList = mapM pom 
        where pom fp = doesFileExist fp 
                   >>= (fp,) . File <$>  readFileLines fp
                     ? pure (fp,Dir)
                       
makeChangeList :: DirStruct -> [FilePath] -> IO [(FilePath,[Diff])]
makeChangeList rec p = filter (not . null . snd) . map fja
                   <$> mapM getContents' (filter isFile p)
                        where isFile = isJust . found
                              getContents' fp = (fp,) <$> readFileLines fp
                              fja (fp,ls)  = (fp, makeDiff (fromMaybe undefined $ found fp) ls)
                              found = contentsToMaybe . fromMaybe undefined . flip M.lookup rec
                     
commit :: Repo -> String -> IO ()
commit r msg = do
        info <- makeCommitInfo r msg
        parents <- getPredCommits r --[Commit] 
        let rec = reconstruct parents  --DirStruct
        p <- genFilePaths r  --[FilePath]
        let (l,b,d) = makeFilePathDiff p $ M.keys rec
        let removeList = makeRemoveList l
        addList <- makeAddList d
        changeList <- makeChangeList rec b
        let com = Commit info removeList changeList addList
        writeCommit r com
                
visit :: Repo -> IO ()
visit r = do 
        parents <- getPredCommits r --[Commit] 
        let rec = reconstruct parents  --DirStruct
        rmfps' <- genFilePaths r
        let rmfps = reverse $ sortPaths $ rmfps'
        mapM_ removePathForcibly rmfps
        let fps = sort' $ M.toList rec
        mapM_ create fps
                where create (fp,(File con)) = writeFile fp $ unlines con
                      create (fp,Dir) = createDirectory fp
                      sort' = sortBy (on cmpPath fst)
                      
                      
