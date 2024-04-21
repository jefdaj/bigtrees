module Cmd.Dupes where

-- TODO guess and check hashes

import Config (Config (..))
import qualified System.Directory.BigTrees as BT

cmdDupes :: Config -> [FilePath] -> IO ()
cmdDupes cfg paths = do
  forest <- BT.readOrBuildTrees (verbose cfg) (maxdepth cfg) (exclude cfg) paths
  -- TODO rewrite sorting with lower memory usage
  -- let dupes = runST $ BT.dupesByNFiles =<< BT.pathsByHash tree
  -- printDupes $ map sortDupePaths $ simplifyDupes BT.dupes
  let ds = BT.dupesByNFiles $ BT.pathsByHash forest
  case txt cfg of
    Nothing -> BT.printDupes (maxdepth cfg) ds
    Just p  -> BT.writeDupes (maxdepth cfg) p ds
