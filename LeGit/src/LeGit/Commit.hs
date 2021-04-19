module LeGit.Commit (nameGen,readFileLines,makeDiff,makeFilePathDiff) where

import Text.JSON
import Crypto.Hash.SHA256
import Text.Hex
import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as B
import LeGit.Basic
import qualified System.IO.Strict as S
import qualified Data.Algorithm.Diff as D
import Data.Function
import Data.Sort
import System.FilePath


--readTree :: Repo -> IO JSValue
--readTree = readJsonFromRepo treeFile JSNull
--          
--writeTree :: Repo -> JSValue -> IO () 
--writeTree = writeJsonToRepo treeFile

nameGen :: JSValue -> String
nameGen = T.unpack . encodeHex . hash . B.fromString . Text.JSON.encode

readFileLines :: FilePath -> IO [String]
readFileLines = fmap lines . S.readFile 

data Diff = Remove { removeIndex :: Int
                   , num :: Int
                   }
          | Add { addIndex :: Int
                , addLines :: [String] 
                }
           deriving(Show,Eq)

--[PolyDiff [(Int,String)] [(Int,String)]]

makeDiff :: [String] -> [String] -> [Diff]
makeDiff old new = map conv 
                 $ filter f 
                 $ D.getGroupedDiffBy ((==) `on` snd) (e old) (e new)
                      where f (D.Both _ _)= False
                            f _ = True
                            e = zip ([1..]::[Int])
                            conv (D.First a) = Remove (fst $ head a) (length a)
                            conv (D.Second a) = Add (fst $ head a) (map snd a)
                            conv _ = undefined

makeFilePathDiff :: [FilePath] -> [FilePath] -> ([FilePath],[FilePath],[FilePath])
makeFilePathDiff old new = foldl fja ([],[],[])
                         $ on D.getDiff sort old new
                                where fja (x,y,z) (D.First n) = (n : x, y, z)
                                      fja (x,y,z) (D.Both n _) = (x, n : y, z)
                                      fja (x,y,z) (D.Second n) = (x, y,n : z) 

cmpPath :: FilePath -> FilePath -> Ordering
cmpPath = on pom splitPath
        where pom (x:xs) (y:ys)
                  | x == y = pom xs ys
                  | otherwise = compare x y
              pom a b = on compare null a b        

sortPaths :: [FilePath] -> [FilePath]
sortPaths = sortBy cmpPath                                 
