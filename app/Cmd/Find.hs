module Cmd.Find where

import Config (Config (..))
-- import System.Directory.BigTrees.HashTree

cmdFind :: Config -> [FilePath] -> IO ()
cmdFind cfg paths = do
  putStrLn "cmdFind"
  putStrLn $ "paths: " ++ show paths
  putStrLn $ "cfg: " ++ show cfg
  -- forest <- readOrBuildTrees (verbose cfg) (maxdepth cfg) (exclude cfg) paths
  -- case txt cfg of
    -- Nothing -> printForest   forest
    -- Just p  -> writeForest p forest
