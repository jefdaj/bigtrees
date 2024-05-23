module Cmd.Find (cmdFind) where

import Config (Config (..))
import System.Directory.BigTrees (readOrBuildTrees, printForestPaths)

cmdFind :: Config -> [FilePath] -> IO ()
cmdFind cfg paths = do
  forest <- readOrBuildTrees (verbose cfg) (maxdepth cfg) (exclude cfg) paths
  printForestPaths forest
