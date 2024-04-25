module OldCmd.Diff where

-- TODO guess and check hashes

import Config (Config (..), defaultConfig)
import qualified Control.Concurrent.Thread.Delay as D
import qualified Data.ByteString.Lazy.UTF8 as BLU
import System.Directory.BigTrees (diff, printDeltas, readOrBuildTree, renameRoot)
import System.Directory.BigTrees.FilePath (absolute)
import System.FilePath (dropExtension, takeBaseName, (</>))
import System.IO (stderr, stdout)
import System.IO.Silently (hCapture)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (cwd, proc, readCreateProcess)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)

oldCmdDiff :: Config -> FilePath -> FilePath -> IO ()
oldCmdDiff cfg old new = do
  tree1 <- renameRoot "old" <$> readOrBuildTree (verbose cfg) (maxdepth cfg) (exclude cfg) old
  tree2 <- renameRoot "new" <$> readOrBuildTree (verbose cfg) (maxdepth cfg) (exclude cfg) new
  printDeltas $ diff tree1 tree2

-----------
-- tests --
-----------

diffTarXz :: FilePath -> FilePath -> IO BLU.ByteString
diffTarXz xz1 xz2 = do
  (Just xz1') <- absolute xz1
  (Just xz2') <- absolute xz2
  withSystemTempDirectory "/tmp/bigtrees" $ \tmpDir -> do
    let d1 = tmpDir </> dropExtension (takeBaseName xz1')
    let d2 = tmpDir </> dropExtension (takeBaseName xz2')
    D.delay 100000 -- wait 0.1 second so we don't capture output from tasty
    _ <- readCreateProcess ((proc "tar" ["-xf", xz1']) {cwd = Just tmpDir}) ""
    _ <- readCreateProcess ((proc "tar" ["-xf", xz2']) {cwd = Just tmpDir}) ""
    (out, ()) <- hCapture [stdout, stderr] $ oldCmdDiff defaultConfig d1 d2
    D.delay 100000 -- wait 0.1 second so we don't capture output from tasty
    return $ BLU.fromString out

test_demo_diff :: TestTree
test_demo_diff =
  let xz1 = "test/app/demo1.tar.xz"
      xz2 = "test/app/demo2.tar.xz"
      gld = "test/app/demo12.diff"
  in goldenVsString
       "diff demo1 -> demo2"
       gld
       (diffTarXz xz1 xz2)
