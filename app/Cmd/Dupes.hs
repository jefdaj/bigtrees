module Cmd.Dupes where

-- TODO guess and check hashes

import Config (Config (..))
import System.Directory.BigTrees (dupesByNFiles, pathsByHash, printDupes, readOrBuildTrees,
                                  writeDupes)

cmdDupes :: Config -> [FilePath] -> IO ()
cmdDupes cfg paths = do
  forest <- readOrBuildTrees (verbose cfg) (maxdepth cfg) (exclude cfg) paths
  -- TODO rewrite sorting with lower memory usage
  -- let dupes = runST $ dupesByNFiles =<< pathsByHash tree
  -- printDupes $ map sortDupePaths $ simplifyDupes dupes
  let ds = dupesByNFiles $ pathsByHash forest
  case txt cfg of
    Nothing -> printDupes (maxdepth cfg) ds
    Just p  -> writeDupes (maxdepth cfg) p ds
