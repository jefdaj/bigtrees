module Cmd.Diff where

-- TODO guess and check hashes

import Config (Config(..))
import System.Directory.BigTrees   (readOrBuildTree, renameRoot, diff, printDeltas)

cmdDiff :: Config -> FilePath -> FilePath -> IO ()
cmdDiff cfg old new = do
  tree1 <- renameRoot "old" <$> readOrBuildTree (verbose cfg) (maxdepth cfg) (exclude cfg) old
  tree2 <- renameRoot "new" <$> readOrBuildTree (verbose cfg) (maxdepth cfg) (exclude cfg) new
  printDeltas $ diff tree1 tree2
