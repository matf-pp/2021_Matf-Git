module LeGit.Commit (makeDiff,makeFilePathDiff) where

import LeGit.Basic
import LeGit.Info
import LeGit.Ignore (getIgnores)

import Data.Function
import Data.Maybe
import System.FilePath
import System.FilePath.Find
import qualified Data.Algorithm.Diff as D
import qualified Data.HashMap.Strict as M

makeCommitInfo :: Repo -> IO (M.HashMap String String)
makeCommitInfo r = do
      u <- (,) "username" <$> getUserNameAssert r
      t <- (,) "time" <$> getTimeString
      e <- map ((,) "email") . catMaybes . pure <$> getInfo "email" r
      return $ M.fromList $ u : t : e

makeDiff :: [String] -> [String] -> [Diff]
makeDiff = fmap (map conv) 
         . fmap (filter f) 
         . on (D.getGroupedDiffBy $ on (==) snd) enumerate
               where f (D.Both _ _)= False
                     f _ = True
                     conv (D.First a) = Remove (fst $ head a) (length a)
                     conv (D.Second a) = Add (fst $ head a) (map snd a)
                     conv _ = undefined

makeFilePathDiff :: [FilePath] -> [FilePath] -> ([FilePath],[FilePath],[FilePath])
makeFilePathDiff = fmap (foldl fja ([],[],[]))
                 . on D.getDiff sortPaths
                        where fja (x,y,z) (D.First n) = (n : x, y, z)
                              fja (x,y,z) (D.Both n _) = (x, n : y, z)
                              fja (x,y,z) (D.Second n) = (x, y,n : z)                          
                               
genFilePaths :: Repo -> IO [FilePath]
genFilePaths r = do
        ignores <- map ((baseDir r) </>) <$> getIgnores r
        let pom = fileName /=? repoDirName &&? fmap (not . flip elem ignores) filePath
        find pom pom $ baseDir r

remove :: DirStruct -> Commit -> DirStruct
remove acc = foldl remove' acc . commitRemoves
        where remove' acc' fp = M.delete fp acc'
        
add :: DirStruct -> Commit -> DirStruct 
add acc com = foldl add' acc (commitAdds com)
        where add' acc' (k,v) = M.insert k v acc' 

change :: DirStruct -> Commit -> DirStruct
change acc = foldl (flip change') acc . commitChanges
        where change' (fp,difs) = M.insert fp $ File $ flip pom difs $ getOld fp acc
              getOld = fmap (fromMaybe undefined) . M.lookup
              pom (File old) = foldl pom' old
              pom Dir = undefined
              pom' old (Remove ind br) = take (ind-1) old ++ drop (ind + br) old
              pom' old (Add ind s) = insertBetween s $ flip splitAt old $ ind-1
              insertBetween s (l,r) = l ++ s ++ r
              
       
reconstruct :: [Commit] -> DirStruct
reconstruct = foldl (flip pom) M.empty 
        where pom com = flip change com . flip add com . flip remove com 
              
