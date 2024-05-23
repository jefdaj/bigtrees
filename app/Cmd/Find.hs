module Cmd.Find
  ( cmdFind
  , cmdFindUnixFind
  , prop_cmdFind_paths_match_unix_find
  )
  where

-- TODO use the actual path passed as the first breadcrumb? would match unix find

import Config (Config (..), defaultConfig)
import Control.Concurrent.Thread.Delay (delay)
import Data.List (sort)
import System.Directory.BigTrees (TestTree, printForestPaths, readOrBuildTrees, writeTestTreeDir)
import System.FilePath (takeBaseName, takeDirectory)
import System.IO (stderr, stdout)
import System.IO.Silently (hCapture)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (cwd, proc, readCreateProcess)
import Test.QuickCheck (Property, arbitrary)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)
-- import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)

cmdFind :: Config -> [FilePath] -> IO ()
cmdFind cfg paths = do
  forest <- readOrBuildTrees (verbose cfg) (maxdepth cfg) (exclude cfg) paths
  printForestPaths (fromMaybe "" $ metafmt cfg) forest

-- Also, sort order turns out to be weirder than I expected with Unicode,
-- so I gave up and sorted the output separately. The important part is
-- that we get the same paths, not necessarily in the same order.
cmdFindUnixFind :: TestTree -> IO (String, String)
cmdFindUnixFind t =
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
    -- by relative path from the parent of the tmpdir to match my relative style.
    let tmpName   = takeBaseName  tmpDir
        tmpParent = takeDirectory tmpDir
    out2 <- readCreateProcess ((proc "find" [tmpName]) {cwd = Just tmpParent}) ""
    let out2' = (unlines . sort . lines) out2
    delay 100000

    return (out1', out2')

prop_cmdFind_paths_match_unix_find :: Property
prop_cmdFind_paths_match_unix_find = monadicIO $ do
  tree <- pick arbitrary
  (out1, out2) <- run $ cmdFindUnixFind tree
  -- WARNING these will mess up your terminal
  -- liftIO $ putStrLn out1
  -- liftIO $ putStrLn out2
  assert $ out1 == out2
