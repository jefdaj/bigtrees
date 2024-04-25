module Cmd.Info where

import Config (Config (..))
import System.Directory.BigTrees (printForest, readOrBuildTrees, writeForest)

cmdInfo :: Config -> [FilePath] -> IO ()
cmdInfo cfg paths = do
  putStrLn "cmdInfo"
  putStrLn $ "paths: " ++ show paths
  putStrLn $ "cfg: " ++ show cfg
  -- forest <- readOrBuildTrees (verbose cfg) (maxdepth cfg) (exclude cfg) paths
  -- case txt cfg of
    -- Nothing -> printForest   forest
    -- Just p  -> writeForest p forest
