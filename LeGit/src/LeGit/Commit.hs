module LeGit.Commit (readFileLines,makeDiff,makeFilePathDiff) where

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
