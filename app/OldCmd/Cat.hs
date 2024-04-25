module OldCmd.Cat where

import Config (Config (..))
import System.Directory.BigTrees (printForest, readOrBuildTrees, writeForest)

oldCmdCat :: Config -> [FilePath] -> IO ()
oldCmdCat cfg paths = do
  forest <- readOrBuildTrees (verbose cfg) (maxdepth cfg) (exclude cfg) paths
  case txt cfg of
    Nothing -> printForest   forest
    Just p  -> writeForest p forest
