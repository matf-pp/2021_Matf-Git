module LeGit.Pointers (initPointers) where

import LeGit.Basic
import Data.Maybe
import qualified Data.HashMap.Strict as M
import Data.Hashable

getShaFromHead :: Pointers -> ShaStr
getShaFromHead (Pointers (Sha h) _ _) = h
getShaFromHead (Pointers (Ref h) r _) = fromMaybe e $ M.lookup h r
    where e = error $ "Internal Error :: Reference " ++ h ++ " does not exist"
getShaFromHead (Pointers (Tag h) _ t) = fromMaybe e $ M.lookup h t
    where e = error $ "Internal Error :: Tag " ++ h ++ " does not exist"

insert' :: (Eq k, Hashable k) => k -> v -> M.HashMap k v -> M.HashMap k v
insert' k v m = pom $ M.lookup k m
    where pom (Just _) = error $ "Element already exists"
          pom Nothing  = M.insert k v m

addTag :: Pointers -> String -> Pointers
addTag p@(Pointers _ r t) name = Pointers (Tag name) r newT
    where newSha = getShaFromHead p
          newT   = M.insert name newSha t

addRef :: Pointers -> String -> Pointers
addRef p@(Pointers _ r t) name = Pointers (Ref name) newR t
    where newSha = getShaFromHead p
          newR   = insert' name newSha r

isCommitable :: Head -> Bool
isCommitable (Ref _) = True
isCommitable _       = False

updateRef :: Pointers -> ShaStr -> Pointers
updateRef (Pointers hh@(Ref h) r t) s = Pointers hh (M.insert h s r) t
updateRef _ _ = error $ "Cannot update when Head is not reference"

initPointers :: Repo -> ShaStr -> IO ()
initPointers r s = writeJsonToRepo pointersFile r
                 $ Pointers (Ref "main") (M.singleton "main" s) M.empty
