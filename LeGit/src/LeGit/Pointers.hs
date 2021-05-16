module LeGit.Pointers (
    getPointers, initState, writeCommit, writeMerge, getPredCommits,
    setHeadFromRef, setHeadFromTag, setHeadFromSha, listRefs, listTags,
    setTag, setRef, removeTag, removeRef, isSha, get3Lists
) where

import LeGit.Basic
import LeGit.Tree

import Data.Maybe
import Data.Either
import Data.List
import qualified Data.HashMap.Strict as M
import Data.Hashable

setHeadFromRef :: Repo -> String -> IO ()
setHeadFromRef r refName = do
    (Pointers _ refsMap tagsMap) <- getPointers r
    if M.member refName refsMap then writePointers r (Pointers (Ref refName) refsMap tagsMap)
                                else errorMsg $ "Ref " ++ refName ++ " does not exist"

setHeadFromTag :: Repo -> String -> IO ()
setHeadFromTag r tagName = do
    (Pointers _ refsMap tagsMap) <- getPointers r
    if M.member tagName tagsMap then writePointers r (Pointers (Tag tagName) refsMap tagsMap)
                                else errorMsg $ "Tag " ++ tagName ++ " does not exist"

setHeadFromSha :: Repo -> String -> IO ()
setHeadFromSha r partialShaStr = do
    shaStrs <- M.keys <$> getTree r
    pom $ filter (isPrefixOf partialShaStr) shaStrs
        where pom shas
                |   null shas = errorMsg $ "String " ++ partialShaStr ++ " is not prefix of any existing sha"
                |   length shas == 1 = do
                        (Pointers _ refsMap tagsMap) <- getPointers r
                        writePointers r (Pointers (Sha $ head shas) refsMap tagsMap)
                |   otherwise = errorMsg $ "Multiple options: " ++ show shas
    
getPointers :: Repo -> IO Pointers
getPointers = readJsonFromRepo pointersFile e
    where e = error "Internal Error :: Failed to parse Pointers file"

writePointers :: Repo -> Pointers -> IO()
writePointers = writeJsonToRepo pointersFile

getShaFromHead :: Pointers -> ShaStr
getShaFromHead (Pointers (Sha h) _ _) = h
getShaFromHead (Pointers (Ref h) r _) = fromMaybe e $ M.lookup h r
    where e = error $ "Internal Error :: Reference " ++ h ++ " does not exist"
getShaFromHead (Pointers (Tag h) _ t) = fromMaybe e $ M.lookup h t
    where e = error $ "Internal Error :: Tag " ++ h ++ " does not exist"

getShaFromRepo :: Repo -> IO ShaStr
getShaFromRepo r = getShaFromHead <$> getPointers r

insert' :: (Eq k, Hashable k) => k -> v -> M.HashMap k v -> M.HashMap k v
insert' k v m = pom $ M.lookup k m
    where pom (Just _) = error "Element already exists"
          pom Nothing  = M.insert k v m

addTag :: Pointers -> String -> Pointers
addTag p@(Pointers h r t) name = Pointers h r newT
    where newSha = getShaFromHead p
          newT   = M.insert name newSha t

setTag :: Repo -> String -> IO ()
setTag r tagName = do
    p <- getPointers r
    let newP = addTag p tagName
    writePointers r newP

removeTag :: Repo -> String -> IO ()
removeTag r s = do
    (Pointers h refsMap tagsMap) <- getPointers r
    if isThisTag h then errorMsg $ "Cannot remove tag " ++ s ++ ": HEAD pointing to it"
                   else writePointers r (Pointers h refsMap $ M.delete s tagsMap)
        where isThisTag (Tag curr) = s == curr
              isThisTag _          = False

addRef :: Pointers -> String -> Pointers
addRef p@(Pointers _ r t) name = Pointers (Ref name) newR t
    where newSha = getShaFromHead p
          newR   = insert' name newSha r

setRef :: Repo -> String -> IO ()
setRef r refName = do
    p <- getPointers r
    let newP = addRef p refName
    writePointers r newP

removeRef :: Repo -> String -> IO ()
removeRef r s = do
    (Pointers h refsMap tagsMap) <- getPointers r
    if isThisRef h then errorMsg $ "Cannot remove branch " ++ s ++ ": HEAD pointing to it"
                   else writePointers r (Pointers h (M.delete s refsMap) tagsMap)
        where isThisRef (Ref curr) = s == curr
              isThisRef _          = False

isCommitable :: Head -> Bool
isCommitable (Ref _) = True
isCommitable _       = False

isSha :: Head -> Bool
isSha (Sha _) = True
isSha _       = False

updateRef :: Pointers -> ShaStr -> Pointers
updateRef (Pointers hh@(Ref h) r t) s = Pointers hh (M.insert h s r) t
updateRef _ _ = error "Cannot update when HEAD is not reference"

initState :: Repo -> IO ()
initState r = do 
    info  <- M.singleton "time" <$> getTimeString
    let c = Commit info $ PureCommit [] [] []
    insertNode r c [] >>= initPointers
        where initPointers s =  writePointers r
                             $ Pointers (Ref "main") (M.singleton "main" s) M.empty

getPredCommits :: Repo -> IO [Commit]
getPredCommits r = getShaFromRepo r >>= getPredecessors r

listRefs :: Repo -> IO ()
listRefs r = do
    (Pointers h refsMap _) <- getPointers r
    mapM_ putStrLn $ pom h $ M.keys refsMap
        where pom (Ref name) xs = map (\x -> if x == name then "~~> " ++ x else x) xs
              pom _ xs          = xs

listTags :: Repo -> IO ()
listTags r = do
    (Pointers h _ tagsMap) <- getPointers r
    mapM_ putStrLn $ pom h $ M.keys tagsMap
        where pom (Tag name) xs = map (\x -> if x == name then "~~> " ++ x else x) xs
              pom _ xs          = xs

writeCommit :: Repo -> Commit -> IO ()
writeCommit r c = do
    p <- getPointers r
    if isCommitable (phead p) then pure () else errorMsg "Cannot update when HEAD is not reference"
    let oldS = getShaFromHead p
    s <- insertNode r c [oldS]
    writePointers r $ updateRef p s

writeMerge :: Repo -> String -> Commit -> IO ()
writeMerge repo name c = do
    secondSha <- getShaFromRepo repo
    setHeadFromRef repo name
    p <- getPointers repo
    let mainSha = getShaFromHead p
    s <- insertNode repo c [mainSha, secondSha]
    writePointers repo $ updateRef p s

get3Lists :: Repo -> String -> IO ([Commit], [Commit], [Commit])
get3Lists r name = do --(rootToCommonInc, commonExcToS1, commonExcToS2) --Inc = including, Exc = excluding
    tree <- getTree r
    p@(Pointers h refsMap _) <- getPointers r
    if isCommitable h then return () else errorMsg "Cannot merge: HEAD is not reference"
    let pom = M.lookup name refsMap
    if isNothing pom then errorMsg $ "Cannot merge: " ++ name ++ " is not a reference" else return ()
    let sha1 = getShaFromHead p
    let sha2 = fromMaybe undefined pom
    let res = getClosestCommonPred tree sha1 sha2
    if isLeft res then errorMsg $ fromLeft undefined res else return ()
    let parent = fromRight undefined res
    let mapm = mapM (flip getPred r)
    parentCommit <- mapm $ getPredecessorsShaStr parent tree
    sha1commit <- mapm $ tail $ dropWhile (/= parent) $ getPredecessorsShaStr sha1 tree
    sha2commit <- mapm $ tail $ dropWhile (/= parent) $ getPredecessorsShaStr sha2 tree
    return (parentCommit, sha1commit, sha2commit)