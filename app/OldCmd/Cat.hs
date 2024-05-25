module OldCmd.Cat where

import Config (Config (..))
import System.Directory.BigTrees (printTree, readOrBuildTree, writeTree)

oldCmdCat :: Config -> FilePath -> IO ()
oldCmdCat cfg path = do
  tree <- readOrBuildTree (verbose cfg) (maxdepth cfg) (exclude cfg) path
  case txt cfg of
    Nothing -> printTree   tree
    Just p  -> writeTree p tree
