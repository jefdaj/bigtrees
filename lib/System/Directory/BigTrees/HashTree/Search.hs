module System.Directory.BigTrees.HashTree.Search where

import Control.Monad (msum)
import Data.Maybe (isJust)
import System.Directory.BigTrees.Hash (Hash)
import System.Directory.BigTrees.HashTree.Base (HashTree(..), NodeData(..))
import System.Directory.BigTrees.Name (fp2n, n2fp)
import System.Directory.BigTrees.Util (pathComponents)
import System.FilePath (joinPath)


-------------------
-- search a tree --
-------------------

-- treeContainsPath :: HashTree -> FilePath -> Bool
-- treeContainsPath (File f1 _     ) f2 = f1 == f2
-- treeContainsPath (Dir  f1 _ cs _) f2
--   | f1 == f2 = True
--   | length (pathComponents f2) < 2 = False
--   | otherwise = let n   = head $ pathComponents f2
--                     f2' = joinPath $ tail $ pathComponents f2
--                 in if f1 /= n
--                   then False
--                   else any (\c -> treeContainsPath c f2') cs

treeContainsPath :: HashTree a -> FilePath -> Bool
treeContainsPath tree path = isJust $ dropTo tree path

dropTo :: HashTree a -> FilePath -> Maybe (HashTree a)
dropTo t@(File {nodeData=nd1}) f2 = if n2fp (name nd1) == f2 then Just t else Nothing
dropTo t@(Dir  {nodeData=nd1, dirContents=cs}) f2
  | n2fp (name nd1) == f2 = Just t
  | length (pathComponents f2) < 2 = Nothing
  | otherwise = let n   = fp2n $ head $ pathComponents f2
                    f2' = joinPath $ tail $ pathComponents f2
                in if (name nd1) /= n
                  then Nothing
                  else msum $ map (`dropTo` f2') cs

treeContainsHash :: HashTree a -> Hash -> Bool
treeContainsHash (File {nodeData=nd1}) h2 = hash nd1 == h2
treeContainsHash (Dir  {nodeData=nd1, dirContents=cs}) h2
  | hash nd1 == h2 = True
  | otherwise = any (`treeContainsHash` h2) cs

-- TODO if tree contains path, be able to extract it! need for rm


