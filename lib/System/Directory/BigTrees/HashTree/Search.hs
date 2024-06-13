module System.Directory.BigTrees.HashTree.Search where

import Control.Monad (msum)
import Data.Maybe (isJust)
import System.Directory.BigTrees.Hash (Hash)
import System.Directory.BigTrees.HashTree.Base (HashTree (..), NodeData (..))
import System.Directory.BigTrees.Name (op2ns)
import System.Directory.BigTrees.Util (pathComponents)
import System.OsPath (joinPath)


-------------------
-- search a tree --
-------------------

-- treeContainsPath :: HashTree -> OsPath -> Bool
-- treeContainsPath (File f1 _     ) f2 = f1 == f2
-- treeContainsPath (Dir  f1 _ cs _) f2
--   | f1 == f2 = True
--   | length (pathComponents f2) < 2 = False
--   | otherwise = let n   = head $ pathComponents f2
--                     f2' = joinPath $ tail $ pathComponents f2
--                 in if f1 /= n
--                   then False
--                   else any (\c -> treeContainsPath c f2') cs

-- TODO is IO here reasonable?
treeContainsPath :: HashTree a -> OsPath -> IO Bool
treeContainsPath tree path = do
  ns <- op2ns path
  isJust $ dropTo tree ns

dropTo :: HashTree a -> [Name] -> Maybe (HashTree a)
dropTo t [] = Just t -- TODO is that right?
dropTo t@(Err {errName=n}) (n2:_) = if n == n2 then Just t else Nothing
dropTo t@(File {nodeData=nd1}) (n2:_) = if name nd1 == n2 then Just t else Nothing
dropTo t@(Dir  {nodeData=nd1, dirContents=cs}) (n:ns)
  | name nd1 /= n = Nothing
  | otherwise = msum $ map (`dropTo` ns) cs

treeContainsHash :: HashTree a -> Hash -> Bool
treeContainsHash (Err {}) _ = False
treeContainsHash (Link {nodeData=nd1}) h2 = hash nd1 == h2
treeContainsHash (File {nodeData=nd1}) h2 = hash nd1 == h2
treeContainsHash (Dir  {nodeData=nd1, dirContents=cs}) h2
  | hash nd1 == h2 = True
  | otherwise = any (`treeContainsHash` h2) cs

-- TODO if tree contains path, be able to extract it! need for rm


