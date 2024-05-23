module Cmd.Find
  ( cmdFind
  , cmdFindVsSortedUnixFind
  , prop_cmdFind_matches_sorted_unix_find
  )
  where

import Config (Config (..), defaultConfig)
import System.Directory.BigTrees (readOrBuildTrees, printForestPaths, TestTree, writeTestTreeDir)
import System.Process (readCreateProcess, shell, cwd)
import Control.Concurrent.Thread.Delay (delay)
import Test.QuickCheck (Property, arbitrary)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)
import System.IO (stderr, stdout)
import System.IO.Silently (hCapture)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath (takeBaseName, takeDirectory)

cmdFind :: Config -> [FilePath] -> IO ()
cmdFind cfg paths = do
  forest <- readOrBuildTrees (verbose cfg) (maxdepth cfg) (exclude cfg) paths
  printForestPaths forest

cmdFindVsSortedUnixFind :: TestTree -> IO (String, String)
cmdFindVsSortedUnixFind t =
  withSystemTempDirectory "bigtrees" $ \tmpDir -> do
    -- tmpDir will be the *parent* of the root tree dir
    writeTestTreeDir tmpDir t
    -- wait 0.1 second before + after each cmd so we don't capture output from tasty
    delay 100000 
    (out1, ()) <- hCapture [stdout, stderr] $ cmdFind defaultConfig [tmpDir]
    delay 100000 
    -- unix find will print whole absolute paths here,
    -- so we need to invoke it from above the tmpDir
    -- TODO print full paths in cmdFind too? or would that be more confusing?
    let tmpName   = takeBaseName  tmpDir
        tmpParent = takeDirectory tmpDir
        unixFind  = "find '" ++ tmpName ++ "' | sort"
    out2 <- readCreateProcess ((shell unixFind) {cwd = Just tmpParent}) ""
    delay 100000 
    return (out1, out2) -- TODO will stderr sometimes print something?

prop_cmdFind_matches_sorted_unix_find :: Property
prop_cmdFind_matches_sorted_unix_find = monadicIO $ do
  tree <- pick arbitrary
  (out1, out2) <- run $ cmdFindVsSortedUnixFind tree
  assert $ out1 == out2
