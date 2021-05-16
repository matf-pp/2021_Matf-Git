{-# LANGUAGE TupleSections #-}
module LeGit.Commit (commit,makeDiff,makeFilePathDiff,visit,status, merge) where

import LeGit.Basic
import LeGit.Info
import LeGit.Ignore (getIgnores)
import LeGit.Pointers

import Data.Function
import Data.Maybe
import Data.Either
import Control.Monad (filterM)
import System.FilePath
import System.FilePath.Find
import qualified Data.Algorithm.Diff as D
import qualified Data.HashMap.Strict as M
import System.Directory
import Data.Sort

makeCommitInfo :: Repo -> String -> IO (M.HashMap String String)
makeCommitInfo r msg = do
      u <- ("username",) <$> getUserNameAssert r
      t <- ("time",) <$> getTimeString
      e <- map ("email",) . catMaybes . pure <$> getInfo "email" r
      let m = ("message", msg)
      return $ M.fromList $ u : t : m : e     

makeDiff :: [String] -> [String] -> [Diff]
makeDiff = fmap (map conv . filter f) . on (D.getGroupedDiffBy $ on (==) snd) enumerate
               where f (D.Both _ _)= False
                     f _ = True
                     conv (D.First a) = Remove (fst $ head a) (length a)
                     conv (D.Second a) = Add (fst $ head a) (map snd a)
                     conv _ = undefined

makeFilePathDiff :: [FilePath] -> [FilePath] -> ([FilePath],[FilePath],[FilePath])   -- new old
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
remove acc = foldl remove' acc . commitRemoves . pureCommit
        where remove' acc' fp = M.delete fp acc'

add :: DirStruct -> Commit -> DirStruct
add acc = foldl add' acc . commitAdds . pureCommit
        where add' acc' (k,v) = M.insert k v acc'

change :: DirStruct -> Commit -> DirStruct
change acc = foldl (flip change') acc . commitChanges . pureCommit
        where change' (fp,difs) = M.insert fp $ File $ flip pom difs $ getOld fp acc
              getOld = fmap (fromMaybe undefined) . M.lookup
              pom (File old) = fst . foldl pom' (old,0)
              pom Dir = undefined
              pom' (old,off) (Remove ind br) = (dropBetween old (ind + off - 1) br, off - br)
              pom' (old,off) (Add ind s) = (insertBetween s old $ ind - 1, off + length s)


reconstruct' :: DirStruct -> [Commit] -> DirStruct
reconstruct' = foldl (flip pom)
        where pom com = flip change com . flip add com . flip remove com
        
reconstruct :: [Commit] -> DirStruct
reconstruct = reconstruct' M.empty      

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
        let com = Commit info $ PureCommit removeList changeList addList
        writeCommit r com
        
status :: Repo -> IO ()
status r = do
        parents <- getPredCommits r --[Commit] 
        let rec = reconstruct parents  --DirStruct
        p <- genFilePaths r  --[FilePath]
        let (l,b',d) = makeFilePathDiff p $ M.keys rec
        let pom fp = not . null . makeDiff (fromMaybe undefined $ contentsToMaybe $ fromMaybe undefined $ M.lookup fp rec) <$> readFileLines fp
        b <- filterM pom b'
        let mapt s = (:) s . map ("\t" ++) 
        mapM_ putStrLn $ mapt "removed:" l ++ mapt "changed:" b ++ mapt "added:" d
                          
                        
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
                 
isMergeable :: [Diff] -> [Diff] -> Bool
isMergeable l r = foldr fja True l
    where fja d1 acc' = if foldr pom True r then acc' else False
                where pom d2 acc = on (pom' acc) toPair d1 d2
                      pom' acc (i1, c1) (i2, c2)
                            | i1 < i2 = if i1+c1 > i2 then False else acc
                            | i1 > i2 = if i2+c2 > i1 then False else acc
                            | otherwise = False
                      toPair (Remove i c) = (i, c)
                      toPair (Add i c)    = (i, length c)
            
                      
makeMergeCommit :: DirStruct -> PureCommit -> PureCommit -> Either [String] PureCommit
makeMergeCommit p (PureCommit r1 c1 a1) (PureCommit r2 c2 a2) = foldl fja (Right $ PureCommit [] [] []) $ M.keys p
    where fja acc fp
            | elem' r1 && isIn c2 = newC c2
            | elem' r1 && isIn a2 = newA a2
            | isIn a1 && elem' r2 = newA a1
            | isIn c1 && elem' r2 = newC c1
            | on (||) elem' r1 r2 = newR
            | isIn a1 && isIn c2  = newE "left add, right change"
            | isIn c1 && isIn a2  = newE "left change, right add"
            | isIn a1 && isIn a2  = newE "left add, right add"
            | isIn c1 && isIn c2  = if on isMergeable get c1 c2 
                                    then newChange acc (fp, on makeChanges get c1 c2)
                                    else newE "left change, right change"
            | isIn c1             = newC c1
            | isIn c2             = newC c2
            | isIn a1             = newA a1
            | isIn a2             = newA a2
            | otherwise           = acc
                where newR  = newRemove acc fp
                      newE  = newError acc . (fp ++) . (": " ++)
                      newC  = newChange acc . (fp,) . get
                      newA  = newAdd acc . (fp,) . get
                      get   = fromMaybe undefined . lookup fp
                      isIn  = elem fp . map fst
                      elem' = elem fp
                      get'  = fromMaybe undefined . contentsToMaybe . fromMaybe undefined . M.lookup fp
                      makeChanges main              = on makeDiff (recFile $ get' p) main . join main
                      recFile ls                    = fst . foldl pom (ls,0)
                      pom (old,off) (Remove ind br) = (dropBetween old (ind + off - 1) br, off - br)
                      pom (old,off) (Add ind s)     = (insertBetween s old $ ind + off, off + length s)
          newRemove (Right (PureCommit r c a)) n = Right $ PureCommit (n : r) c a
          newRemove acc _                        = acc
          newChange (Right (PureCommit r c a)) n = Right $ PureCommit r (n : c) a
          newChange acc _                        = acc
          newAdd (Right (PureCommit r c a)) n    = Right $ PureCommit r c (n : a)
          newAdd acc _                           = acc
          newError (Left xs) n                   = Left $  n : xs
          newError _ n                           = Left [n]
          join = fmap sort . (++)
                   
             
merge :: Repo -> String -> String -> IO ()
merge r s msg = do
          info <- makeCommitInfo r msg
          (a,b,c) <- get3Lists r s --b grana, c main 
          let parRec = reconstruct a
          let rez = on (makeMergeCommit parRec) (merge' parRec) c b
          if isRight rez then (writeMerge r s $ Commit info $ fromRight undefined rez) >> visit r
                         else mapM_ putStrLn $ fromLeft undefined rez

merge' :: DirStruct -> [Commit] -> PureCommit
merge' parRec child = pc $ map' $ on makeFilePathDiff M.keys childRec parRec 
        where childRec       = reconstruct' parRec child
              pc (r, c, a)   = PureCommit r c a
              map' (l, b, d) = (makeRemoveList l, makeMergeChangeList parRec childRec b, makeMergeAddList parRec d)
         

makeMergeAddList  :: DirStruct -> [FilePath] -> [(FilePath,Contents)]
makeMergeAddList par = map pom
        where pom fp = (fp,fromMaybe undefined $ M.lookup fp par)

makeMergeChangeList :: DirStruct -> DirStruct -> [FilePath] -> [(FilePath,[Diff])]
makeMergeChangeList par child = map pom
        where pom fp = (fp, on makeDiff find'' par child)
                where find'   = fromMaybe undefined . contentsToMaybe . fromMaybe undefined . M.lookup fp
                      filter' = M.filter $ isJust . contentsToMaybe
                      find''  = find' . filter'

         
