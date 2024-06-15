module OldCmd.Cat where

import Config (Config (..))
import System.Directory.BigTrees (printTree, readOrBuildTree, writeTree)
import System.OsPath (OsPath)

oldCmdCat :: Config -> OsPath -> IO ()
oldCmdCat cfg path = do
  tree <- readOrBuildTree (verbose cfg) (maxdepth cfg) (exclude cfg) path
  case txt cfg of
    Nothing -> printTree   tree
    Just p  -> writeTree (exclude cfg) p tree
