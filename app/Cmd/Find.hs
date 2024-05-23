module Cmd.Find
  ( cmdFind
  , printForestPaths
  , printTreePaths
  , printPath
  )
  where

import Config (Config (..))
import System.FilePath ((</>))
import System.Directory.BigTrees (HashTree(..), HashForest(..), Name, readOrBuildTrees, n2fp)


cmdFind :: Config -> [FilePath] -> IO ()
cmdFind cfg paths = do
  putStrLn "cmdFind"
  putStrLn $ "paths: " ++ show paths
  putStrLn $ "cfg: " ++ show cfg
  forest <- readOrBuildTrees (verbose cfg) (maxdepth cfg) (exclude cfg) paths
  putStrLn $ show forest
  printForestPaths forest

printForestPaths :: HashForest a -> IO ()
printForestPaths (HashForest ts) = mapM_ printTreePaths ts

printTreePaths :: HashTree a -> IO ()
printTreePaths = printTreePaths' []

-- Recursively print paths, passing a list of breadcrumbs.
-- Note that the breadcrumbs are in reverse order so `cons`ing is easy
-- TODO is there a performance difference between breadcrumbs and full paths?
-- TODO take an additional "anchor" or "root" arg? or make that the first breadcrumb?
printTreePaths' :: [Name] -> HashTree a -> IO ()
printTreePaths' ns t = do
  let ns' = name t:ns
  printPath ns'
  case t of
    (Dir {}) -> mapM_ (printTreePaths' ns') (contents t) -- TODO sort contents by name? hash?
    _ -> return ()

-- Note that the Names are in reverse order here, so we flip to undo that.
printPath :: [Name] -> IO ()
printPath = putStrLn . foldr1 (flip (</>)) . map n2fp
