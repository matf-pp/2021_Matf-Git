{-# LANGUAGE TupleSections #-}
module LeGit.Commit (commit,makeDiff,makeFilePathDiff,visit,status) where

import LeGit.Basic
import LeGit.Info
import LeGit.Ignore (getIgnores)
import LeGit.Pointers

import Data.Function
import Data.Maybe
import Data.Either
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
              pom' (old,off) (Remove ind br) = (take (ind-1 + off) old ++ drop (ind -1 + br + off) old,off - br)
              pom' (old,off) (Add ind s) = (insertBetween s $ flip splitAt old $ ind-1,off + length s)
              insertBetween s (l,r) = l ++ s ++ r


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
        let (l,b,d) = makeFilePathDiff p $ M.keys rec
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
    where fja d1 acc = if fst (foldr pom (True,d1) r) then acc && True else False
          pom d2 (acc,d1) = (pom' acc d1 d2,d1)
          pom' acc (Remove i1 c1) (Remove i2 c2) = pom'' acc i1 c1 i2 c2
          pom' acc (Remove i1 c1) (Add i2 c2) = pom'' acc i1 c1 i2 (length c2)
          pom' acc (Add i1 c1) (Remove i2 c2) = pom'' acc i1 (length c1) i2 c2
          pom' acc (Add i1 c1) (Add i2 c2) = pom'' acc i1 (length c1) i2 (length c2)
          pom'' acc i1 c1 i2 c2
            | i1 < i2 && i1+c1 > i2 = False
            | i1 < i2 && i1+c1 <= i2 = acc && True
            | i1 > i2 && i2+c2 <= i1 = acc && True
            | i1 > i2 && i2+c2 > i1 = False
            | otherwise = False 
            
                      
makeMergeCommit :: DirStruct -> PureCommit -> PureCommit -> Either [String] PureCommit
makeMergeCommit p (PureCommit r1 c1 a1) (PureCommit r2 c2 a2) = conv $ foldl fja ([],[],[],[]) (M.toList p)
    where fja (r,c,a,x) (fp,_)
            | elem fp r1 && elem fp r2 = (r ++ [fp],c,a,x)
            | elem fp r1 && isIn fp c2 = (r,c ++ [(fp,get fp c2)],a,x)
            | elem fp r1 && isIn fp a2 = (r,c,a ++ [(fp,get fp a2)],x)
            | isIn fp a1 && elem fp r2 = (r,c,a ++ [(fp,get fp a1)],x)
            | isIn fp a1 && isIn fp c2 = (r,c,a,x ++ [ fp ++ ": left add, right change"])
            | isIn fp a1 && isIn fp a2 = (r,c,a,x ++ [ fp ++ ": left add, right add"])
            | isIn fp c1 && elem fp r2 = (r,c ++ [(fp,get fp c1)],a,x)
            | isIn fp c1 && isIn fp c2 = if on isMergeable (get fp) c1 c2 then (r,join c1 c2,a,x) else (r,c,a,x ++ [fp ++ ": left change, right change"])
            | isIn fp c1 && isIn fp a2 = (r,c,a,x ++ [fp ++ ": left change, right add"])
            | isIn fp c1               = (r,c ++ [(fp,get fp c1)],a,x)
            | isIn fp c2               = (r,c ++ [(fp,get fp c2)],a,x)
            | elem fp r1 || elem fp r2 = (r ++ [fp],c,a,x)
            | isIn fp a1               = (r,c,a ++ [(fp,get fp a1)],x)
            | isIn fp a2               = (r,c,a ++ [(fp,get fp a2)],x)
            | otherwise                = (r,c,a,x) 
          isIn fp = elem fp . map fst
          get = fmap (fromMaybe undefined) . lookup
          conv (x,y,z,[]) = Right $ PureCommit x y z
          conv (_,_,_,err) = Left err
          join = fmap sort . (++)
             
merge :: Repo -> String -> String -> IO ()
merge r s msg = do
          info <- makeCommitInfo r msg
          (a,b,c) <- get3Lists r s
          let parRec = reconstruct a
          let bRec = reconstruct' parRec b
          let cRec = reconstruct' parRec c
          let (lb,bb,db) = on makeFilePathDiff M.keys parRec bRec
          let (lc,bc,dc) = on makeFilePathDiff M.keys parRec bRec
          let rb = makeRemoveList lb
          cb <- makeChangeList parRec bb
          ab <- makeAddList db
          let rc = makeRemoveList lc
          cc <- makeChangeList parRec bc
          ac <- makeAddList dc
          let blists = PureCommit rb cb ab         
          let clists = PureCommit rc cc ac
          let rez = makeMergeCommit parRec blists clists
          if isRight rez then writeMerge r s (Commit info (fromRight undefined rez))
                         else mapM_ putStrLn (fromLeft undefined rez)
