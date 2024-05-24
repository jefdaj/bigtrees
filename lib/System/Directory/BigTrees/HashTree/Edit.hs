module System.Directory.BigTrees.HashTree.Edit
  ( addSubTree
  , rmSubTree
  , wrapInEmptyDir
  , wrapInEmptyDirs
  )
  where

import Data.Either (fromRight)
import Data.Function (on)
import Data.List (delete, find, sortBy)
import System.Directory.BigTrees.HashLine (Size(..))
import System.Directory.BigTrees.HashTree.Base (HashTree(..), NodeData(..),
                                                sumNodes, hashContents)
import System.Directory.BigTrees.HashTree.Search (dropTo)
import System.Directory.BigTrees.HashTree.Write ()
import System.Directory.BigTrees.Name (fp2n)
import System.Directory.BigTrees.Util (pathComponents)
import System.FilePath (joinPath, splitPath)


-------------------
-- add a subtree --
-------------------

-- TODO use this to implement hashing multiple trees at once?
-- TODO is the mod time right?
wrapInEmptyDir :: FilePath -> HashTree a -> HashTree a
wrapInEmptyDir n t = Dir
  { dirContents = cs
  , nNodes  = sumNodes t + 1
  , nodeData = NodeData
    { name     = fp2n n
    , hash     = h
    , modTime  = modTime $ nodeData t
    , size     = size (nodeData t) + Size 4096 -- TODO does this vary?
    }
  }
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
addSubTree (File {}) _ _ = error "attempt to insert tree into a file"
addSubTree _ _ path | null (pathComponents path) = error "can't insert tree at null path"
addSubTree main sub path = main { nodeData = nd', dirContents = cs', nNodes = n' }
  where
    comps  = pathComponents path
    p1     = head comps
    path'  = joinPath $ tail comps
    h'     = hashContents cs'
    nd'    = (nodeData main) { hash = h', modTime = mt', size = s' }
    cs'    = sortBy
               (compare `on` (name . nodeData)) $
               filter
                 (\c -> (name . nodeData) c /= fp2n p1)
                 ((dirContents main) ++ [newSub])
    n'     = sumNodes main + sumNodes newSub - maybe 0 sumNodes oldSub
    s'     = size (nodeData main) + size (nodeData newSub) - (maybe 0 (size . nodeData) oldSub)
    mt'    = maximum $ map (modTime . nodeData) [main, newSub]
    sub'   = sub { nodeData=(nodeData sub) {name = fp2n $ last comps }}
    oldSub = find (\c -> (name . nodeData) c == fp2n p1) (dirContents main)
    newSub = if length comps == 1
               then sub'
               else case oldSub of
                 Nothing -> wrapInEmptyDirs path sub'
                 Just d  -> addSubTree d sub' path'

----------------------
-- remove a subtree --
----------------------

{- This one gets a little complicated because if the subtree exists
 - then after removing it we have to adjust parent nNodes back up to the root.
 - Also edits have to be done on the parent tree (so no File branch).
 - Buuuut for now can just ignore nNodes as it's not needed for the rm itself.
 - TODO does this actually solve nNodes too?
 -}
rmSubTree :: HashTree a -> FilePath -> Either String (HashTree a)
rmSubTree (File {}) p = Left $ "no such subtree: '" ++ p ++ "'"
rmSubTree d@(Dir {dirContents=cs, nNodes=n}) p = case dropTo d p of
  Nothing -> Left $ "no such subtree: '" ++ p ++ "'"
  Just t -> Right $ if t `elem` cs
    then d { dirContents = delete t cs, nNodes = n - sumNodes t }
    else d { dirContents = map (\c -> fromRight c $ rmSubTree c $ joinPath $ tail $ splitPath p) cs
           , nNodes = n - sumNodes t
           }
