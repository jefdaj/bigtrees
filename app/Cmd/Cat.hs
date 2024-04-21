module Cmd.Cat where

import           Config                    (Config (..))
import           System.Directory.BigTrees (printForest, readOrBuildTrees,
                                            writeForest)

cmdCat :: Config -> [FilePath] -> IO ()
cmdCat cfg paths = do
  forest <- readOrBuildTrees (verbose cfg) (maxdepth cfg) (exclude cfg) paths
  case txt cfg of
    Nothing -> printForest   forest
    Just p  -> writeForest p forest
