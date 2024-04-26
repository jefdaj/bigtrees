module System.Directory.BigTrees.HashTree.Search where

import Control.Monad (msum)
import Data.Maybe (isJust)
import System.Directory.BigTrees.FilePath (fp2n, n2fp, pathComponents)
import System.Directory.BigTrees.Hash (Hash)
import System.Directory.BigTrees.HashTree.Base (HashTree (Dir, File), ProdTree)
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

treeContainsPath :: ProdTree -> FilePath -> Bool
treeContainsPath tree path = isJust $ dropTo tree path

dropTo :: ProdTree -> FilePath -> Maybe ProdTree
dropTo t@(File f1 _ ()  ) f2 = if n2fp f1 == f2 then Just t else Nothing
dropTo t@(Dir  f1 _ cs _) f2
  | n2fp f1 == f2 = Just t
  | length (pathComponents f2) < 2 = Nothing
  | otherwise = let n   = fp2n $ head $ pathComponents f2
                    f2' = joinPath $ tail $ pathComponents f2
                in if f1 /= n
                  then Nothing
                  else msum $ map (`dropTo` f2') cs

treeContainsHash :: ProdTree -> Hash -> Bool
treeContainsHash (File _ h1 ()  ) h2 = h1 == h2
treeContainsHash (Dir  _ h1 cs _) h2
  | h1 == h2 = True
  | otherwise = any (`treeContainsHash` h2) cs

-- TODO if tree contains path, be able to extract it! need for rm


