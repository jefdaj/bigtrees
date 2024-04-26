module Cmd.Info where

import Config (Config (..))
-- import System.Directory.BigTrees.HashTree.Base

cmdInfo :: Config -> [FilePath] -> IO ()
cmdInfo cfg paths = do
  putStrLn "cmdInfo"
  putStrLn $ "paths: " ++ show paths
  putStrLn $ "cfg: " ++ show cfg
  -- forest <- readOrBuildTrees (verbose cfg) (maxdepth cfg) (exclude cfg) paths
  -- case txt cfg of
    -- Nothing -> printForest   forest
    -- Just p  -> writeForest p forest
