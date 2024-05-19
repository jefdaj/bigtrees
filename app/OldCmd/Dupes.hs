module OldCmd.Dupes where

-- TODO guess and check hashes

import Config (Config (..), defaultConfig)
import qualified Control.Concurrent.Thread.Delay as D
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified System.Directory.BigTrees as BT
import System.Directory.BigTrees.Path (absolute)
import System.FilePath (dropExtension, takeBaseName, (</>))
import System.IO (stderr, stdout)
import System.IO.Silently (hCapture)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (cwd, proc, readCreateProcess)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)

oldCmdDupes :: Config -> [FilePath] -> IO ()
oldCmdDupes cfg paths = do
  forest <- BT.readOrBuildTrees (verbose cfg) (maxdepth cfg) (exclude cfg) paths
  -- TODO rewrite sorting with lower memory usage
  -- let dupes = runST $ BT.dupesByNFiles =<< BT.pathsByHash tree
  -- printDupes $ map sortDupePaths $ simplifyDupes BT.dupes
  let ds = BT.dupesByNFiles $ BT.pathsByHash forest
  case txt cfg of
    Nothing -> BT.printDupes (maxdepth cfg) ds
    Just p  -> BT.writeDupes (maxdepth cfg) p ds

-----------
-- tests --
-----------

dupesTarXz :: FilePath -> FilePath -> IO BLU.ByteString
dupesTarXz xz1 xz2 = do
  (Just xz1') <- absolute xz1
  (Just xz2') <- absolute xz2
  withSystemTempDirectory "/tmp/bigtrees" $ \tmpDir -> do
    let d1 = tmpDir </> dropExtension (takeBaseName xz1')
    let d2 = tmpDir </> dropExtension (takeBaseName xz2')
    D.delay 100000 -- wait 0.1 second so we don't capture output from tasty
    _ <- readCreateProcess ((proc "tar" ["-xf", xz1']) {cwd = Just tmpDir}) ""
    _ <- readCreateProcess ((proc "tar" ["-xf", xz2']) {cwd = Just tmpDir}) ""
    (out, ()) <- hCapture [stdout, stderr] $ oldCmdDupes defaultConfig [d1, d2]
    D.delay 100000 -- wait 0.1 second so we don't capture output from tasty
    return $ BLU.fromString out

test_demo_dupes :: TestTree
test_demo_dupes =
  let xz1 = "test/app/demo1.tar.xz"
      xz2 = "test/app/demo2.tar.xz"
      gld = "test/app/demo12.dupes"
  in goldenVsString
       "dupes demo1 + demo2"
       gld
       (dupesTarXz xz1 xz2)
