module System.Directory.BigTrees.HashTree.Edit where

import Data.Either (fromRight)
import Data.Function (on)
import Data.List (delete, find, sortBy)
import System.Directory.BigTrees.HashTree.Base (HashTree (Dir, File, contents, hash, nFiles, name),
                                                ProdTree, countFiles, hashContents)
import System.Directory.BigTrees.HashTree.Search (dropTo)
import System.Directory.BigTrees.HashTree.Write ()
import System.Directory.BigTrees.Name (fp2n)
import System.Directory.BigTrees.Util (pathComponents)
import System.FilePath (joinPath, splitPath)


-------------------
-- add a subtree --
-------------------

-- TODO use this to implement hashing multiple trees at once?
wrapInEmptyDir :: FilePath -> HashTree a -> HashTree a
wrapInEmptyDir n t = Dir { name = fp2n n, hash = h, contents = cs, nFiles = nFiles t }
  where
    cs = [t]
    h = hashContents cs

wrapInEmptyDirs :: FilePath -> HashTree a -> HashTree a
wrapInEmptyDirs p t = case pathComponents p of
  []     -> error "wrapInEmptyDirs needs at least one dir"
  [n]    -> wrapInEmptyDir n t
  (n:ns) -> wrapInEmptyDir n $ wrapInEmptyDirs (joinPath ns) t

-- TODO does the anchor here matter? maybe it's set to the full path accidentally
addSubTree :: HashTree a -> HashTree a -> FilePath -> HashTree a
addSubTree (File _ _ _) _ _ = error "attempt to insert tree into a file"
addSubTree _ _ path | null (pathComponents path) = error "can't insert tree at null path"
addSubTree main sub path = main { hash = h', contents = cs', nFiles = n' }
  where
    comps  = pathComponents path
    p1     = head comps
    path'  = joinPath $ tail comps
    h'     = hashContents cs'
    cs'    = sortBy (compare `on` name) $ filter (\c -> name c /= fp2n p1) (contents main) ++ [newSub]
    n'     = nFiles main + nFiles newSub - maybe 0 nFiles oldSub
    sub'   = sub { name = fp2n $ last comps }
    oldSub = find (\c -> name c == fp2n p1) (contents main)
    newSub = if length comps == 1
               then sub'
               else case oldSub of
                 Nothing -> wrapInEmptyDirs path sub'
                 Just d  -> addSubTree d sub' path'

----------------------
-- remove a subtree --
----------------------

{- This one gets a little complicated because if the subtree exists
 - then after removing it we have to adjust parent nFiles back up to the root.
 - Also edits have to be done on the parent tree (so no File branch).
 - Buuuut for now can just ignore nFiles as it's not needed for the rm itself.
 - TODO does this actually solve nFiles too?
 -}
rmSubTree :: HashTree a -> FilePath -> Either String (HashTree a)
rmSubTree (File _ _ _) p = Left $ "no such subtree: '" ++ p ++ "'"
rmSubTree d@(Dir _ _ cs n) p = case dropTo d p of
  Nothing -> Left $ "no such subtree: '" ++ p ++ "'"
  Just t -> Right $ if t `elem` cs
    then d { contents = delete t cs, nFiles = n - countFiles t }
    else d { contents = map (\c -> fromRight c $ rmSubTree c $ joinPath $ tail $ splitPath p) cs
           , nFiles = n - countFiles t
           }
