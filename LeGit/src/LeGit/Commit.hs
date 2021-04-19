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
makeDiff = fmap (map conv) 
         . fmap (filter f) 
         . on (D.getGroupedDiffBy $ on (==) snd) e
               where f (D.Both _ _)= False
                     f _ = True
                     e = zip ([1..]::[Int])
                     conv (D.First a) = Remove (fst $ head a) (length a)
                     conv (D.Second a) = Add (fst $ head a) (map snd a)
                     conv _ = undefined

makeFilePathDiff :: [FilePath] -> [FilePath] -> ([FilePath],[FilePath],[FilePath])
makeFilePathDiff = fmap (foldl fja ([],[],[]))
                 . on D.getDiff sortPaths
                        where fja (x,y,z) (D.First n) = (n : x, y, z)
                              fja (x,y,z) (D.Both n _) = (x, n : y, z)
                              fja (x,y,z) (D.Second n) = (x, y,n : z)                            
