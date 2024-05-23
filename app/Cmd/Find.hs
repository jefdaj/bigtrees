module Cmd.Find
  ( cmdFind
  , cmdFindVsUnixFind
  , prop_cmdFind_matches_unix_find
  )
  where

import Config (Config (..), defaultConfig)
import System.Directory.BigTrees (readOrBuildTrees, printForestPaths, TestTree, writeTestTreeDir)
import System.Process (readCreateProcess, proc, cwd)
import Control.Concurrent.Thread.Delay (delay)
import Test.QuickCheck (Property, arbitrary)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)
import System.IO (stderr, stdout)
import System.IO.Silently (hCapture)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath (takeBaseName, takeDirectory)
import Data.List (sort)
-- import Control.Monad.IO.Class (liftIO)

cmdFind :: Config -> [FilePath] -> IO ()
cmdFind cfg paths = do
  forest <- readOrBuildTrees (verbose cfg) (maxdepth cfg) (exclude cfg) paths
  printForestPaths forest

cmdFindVsUnixFind :: TestTree -> IO (String, String)
cmdFindVsUnixFind t =
  withSystemTempDirectory "bigtrees" $ \tmpDir -> do

    -- tmpDir will be the *parent* of the root tree dir
    writeTestTreeDir tmpDir t
    -- wait 0.1 second before + after each cmd so we don't capture output from tasty
    delay 100000

    (out1, ()) <- hCapture [stdout, stderr] $ cmdFind defaultConfig [tmpDir]
    -- TODO why is this necessary? what's different vs `sortOn name`?
    let out1' = (unlines . sort . lines) out1
    delay 100000

    -- Unix find will print whole absolute paths here, so we need to invoke it
    -- from above the tmpDir.
    --
    -- Also, sort order turns out to be weirder than I expected with Unicode,
    -- so I gave up and sorted the output separately. The only important test
    -- is whether we get all the same paths.
    --
    let tmpName   = takeBaseName  tmpDir
        tmpParent = takeDirectory tmpDir
    out2 <- readCreateProcess ((proc "find" [tmpName]) {cwd = Just tmpParent}) ""
    let out2' = (unlines . sort . lines) out2
    delay 100000

    return (out1', out2') -- TODO will there ever be stderr?

prop_cmdFind_matches_unix_find :: Property
prop_cmdFind_matches_unix_find = monadicIO $ do
  tree <- pick arbitrary
  (out1, out2) <- run $ cmdFindVsUnixFind tree
  -- WARNING these will mess up your terminal
  -- liftIO $ putStrLn out1
  -- liftIO $ putStrLn out2
  assert $ out1 == out2
