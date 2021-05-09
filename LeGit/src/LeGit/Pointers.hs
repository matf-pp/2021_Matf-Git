module LeGit.Pointers (
    getPointers, initState, writeCommit, getPredCommits,
    setHeadFromRef, setHeadFromTag, setHeadFromSha,
    setTag, setRef, removeTag, removeRef, isSha
) where

import LeGit.Basic
import LeGit.Tree

import Data.Maybe
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
    if isThisTag h s then errorMsg $ "Cannot remove tag " ++ s ++ ": Head pointing to it"
                     else writePointers r (Pointers h refsMap $ M.delete s tagsMap)
        where isThisTag (Tag curr) s = s == curr
              isThisTag _ _          = False

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
    if isThisRef h s then errorMsg $ "Cannot remove branch " ++ s ++ ": Head pointing to it"
                     else writePointers r (Pointers h (M.delete s refsMap) tagsMap)
        where isThisRef (Ref curr) s = s == curr
              isThisRef _ _          = False

isCommitable :: Head -> Bool
isCommitable (Ref _) = True
isCommitable _       = False

isSha :: Head -> Bool
isSha (Sha _) = True
isSha _       = False

updateRef :: Pointers -> ShaStr -> Pointers
updateRef (Pointers hh@(Ref h) r t) s = Pointers hh (M.insert h s r) t
updateRef _ _ = error "Cannot update when Head is not reference"

initState :: Repo -> IO ()
initState r = do 
    info  <- M.singleton "time" <$> getTimeString
    let c = Commit info $ PureCommit [] [] []
    insertNode r c [] >>= initPointers
        where initPointers s =  writePointers r
                             $ Pointers (Ref "main") (M.singleton "main" s) M.empty

getPredCommits :: Repo -> IO [Commit]
getPredCommits r = getShaFromRepo r >>= getPredecessors r

writeCommit :: Repo -> Commit -> IO ()
writeCommit r c = do
    p <- getPointers r
    if isCommitable (phead p) then pure () else errorMsg "Cannot update when Head is not reference"
    let oldS = getShaFromHead p
    s <- insertNode r c [oldS]
    writePointers r $ updateRef p s
