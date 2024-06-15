module OldCmd.Update where

-- TODO guess and check hashes? not sure if that makes sense here
-- TODO make an operation to replace a subtree (insert despite it existing, then diff)

import Config (Config (..))
import System.Directory.BigTrees (addSubTree, printTree, readOrBuildTree, op2ns)
import System.OsPath (OsPath)

oldCmdUpdate :: Config -> OsPath -> OsPath -> OsPath -> IO ()
oldCmdUpdate cfg root sub path = do
  tree1 <- readOrBuildTree (verbose cfg) (maxdepth cfg) (exclude cfg) root
  tree2 <- readOrBuildTree (verbose cfg) (maxdepth cfg) (exclude cfg) sub
  printTree $ addSubTree tree1 tree2 $ op2ns path
