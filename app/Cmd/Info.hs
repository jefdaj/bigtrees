module Cmd.Info where

import Config (Config (..))
-- import System.Directory.BigTrees.HashTree

cmdInfo :: Config -> FilePath -> IO ()
cmdInfo cfg path = do
  putStrLn "cmdInfo"
  putStrLn $ "path: " ++ show path
  putStrLn $ "cfg: " ++ show cfg
  -- forest <- readOrBuildTrees (verbose cfg) (maxdepth cfg) (exclude cfg) paths
  -- case txt cfg of
    -- Nothing -> printForest   forest
    -- Just p  -> writeForest p forest
