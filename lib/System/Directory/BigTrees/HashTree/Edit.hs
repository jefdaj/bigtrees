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
import System.Directory.BigTrees.HashLine (NBytes (..))
import System.Directory.BigTrees.HashTree.Base (HashTree (..), NodeData (..), hashContents,
                                                sumNodes, treeModTime, treeNBytes, treeName)
import System.Directory.BigTrees.HashTree.Search (dropTo)
import System.Directory.BigTrees.HashTree.Write ()
import System.Directory.BigTrees.Name (Name, fp2n)
import System.Directory.BigTrees.Util (pathComponents)
import System.OsPath (OsPath, joinPath, splitPath)


-------------------
-- add a subtree --
-------------------

-- TODO use this to implement hashing multiple trees at once?
-- TODO is the mod time right?
wrapInEmptyDir :: Name -> HashTree a -> HashTree a
wrapInEmptyDir n t = Dir
  { dirContents = cs
  , nNodes  = sumNodes t + 1
  , nodeData = NodeData
    { name     = n
    , hash     = h
    , modTime  = treeModTime t
    , nBytes   = treeNBytes t + NBytes 4096 -- TODO how to determine this??
    }
  }
  where
    cs = [t]
    h = hashContents cs

wrapInEmptyDirs :: [Name] -> HashTree a -> HashTree a
wrapInEmptyDirs []     _ = error "wrapInEmptyDirs needs at least one dir"
wrapInEmptyDirs [n]    t = wrapInEmptyDir n t
wrapInEmptyDirs (n:ns) t = wrapInEmptyDir n $ wrapInEmptyDirs ns t

-- TODO does the anchor here matter? maybe it's set to the full path accidentally
addSubTree :: HashTree a -> HashTree a -> [Name] -> HashTree a
addSubTree (Err  {}) sub [] = sub -- TODO is this right?
addSubTree (File {}) _ _ = error "attempt to insert tree into a file"
addSubTree _ _ [] = error "can't insert tree at null path"
addSubTree main sub (n:ns) = main { nodeData = nd', dirContents = cs', nNodes = n' }
  where
    -- comps  = pathComponents path
    comps  = ns
    -- p1     = n
    -- path'  = joinPath comps
    h'     = hashContents cs'
    nd'    = (nodeData main) { hash = h', modTime = mt', nBytes = s' }
    cs'    = sortBy
               (compare `on` treeName) $
               filter
                 (\c -> treeName c /= n)
                 ((dirContents main) ++ [newSub])
    n'     = sumNodes main + sumNodes newSub - maybe 0 sumNodes oldSub
    s'     = treeNBytes main + treeNBytes newSub - (maybe 0 treeNBytes oldSub)
    mt'    = maximum $ map treeModTime [main, newSub]
    sub'   = sub { nodeData=(nodeData sub) {name = n}}
    oldSub = find (\c -> treeName c == n) (dirContents main)
    -- TODO is this at all right anymore?
    newSub = if null ns
               then sub'
               else case oldSub of
                 Nothing -> wrapInEmptyDirs ns sub'
                 Just d  -> addSubTree d sub' ns

----------------------
-- remove a subtree --
----------------------

{- This one gets a little complicated because if the subtree exists
 - then after removing it we have to adjust parent nNodes back up to the root.
 - Also edits have to be done on the parent tree (so no File branch).
 - Buuuut for now can just ignore nNodes as it's not needed for the rm itself.
 - TODO does this actually solve nNodes too?
 -}
rmSubTree :: Eq a => HashTree a -> [Name] -> Either String (HashTree a)
rmSubTree (Err  {}) ns = Left $ "no such subtree: " ++ show ns -- TODO is this right?
rmSubTree (File {}) ns = Left $ "no such subtree: " ++ show ns
rmSubTree d@(Dir {dirContents=cs, nNodes=nn}) (n:ns) = case dropTo d ns of
  Nothing -> Left $ "no such subtree: " ++ show (n:ns)
  Just t -> Right $ if t `elem` cs
    then d { dirContents = delete t cs, nNodes = nn - sumNodes t }
    else d { dirContents = map (\c -> fromRight c $ rmSubTree c ns) cs
           , nNodes = nn - sumNodes t
           }
