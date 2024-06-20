module Cmd.Dupes where

-- TODO guess and check hashes

import Config (AppConfig (..), defaultAppConfig)
import qualified Control.Concurrent.Thread.Delay as D
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified System.Directory as SD
import qualified System.Directory.BigTrees as BT
import System.FilePath (dropExtension, takeBaseName, (</>))
import System.IO (stderr, stdout)
import System.IO.Silently (hCapture)
import System.IO.Temp (withSystemTempDirectory)
import System.OsPath (OsPath, encodeFS)
import System.Process (cwd, proc, readCreateProcess)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)

cmdDupes :: AppConfig -> OsPath -> IO ()
cmdDupes cfg path = do
  tree <- BT.readOrBuildTree (searchCfg cfg) (verbose cfg) path
  -- TODO rewrite sorting with lower memory usage
  -- let dupes = runST $ BT.dupesByNNodes =<< BT.pathsByHash tree
  -- printDupes $ map sortDupePaths $ simplifyDupes BT.dupes
  let ds = BT.dupesByNNodes $ BT.pathsByHash tree
  -- TODO handle backet thing here instead?
  case outFile cfg of
    Nothing -> BT.printDupes (searchCfg cfg) ds
    Just p  -> BT.writeDupes (searchCfg cfg) p ds

-----------
-- tests --
-----------

-- TODO make a bigger test with grafted trees to replace the two-demo one that was here
dupesTarXz :: FilePath -> IO BLU.ByteString
dupesTarXz xz1 = do
  xz1' <- SD.makeAbsolute xz1
  withSystemTempDirectory "bigtrees" $ \tmpDir -> do
    let d1 = tmpDir </> dropExtension (takeBaseName xz1)
    d1' <- encodeFS d1
    D.delay 100000 -- wait 0.1 second so we don't capture output from tasty
    _ <- readCreateProcess ((proc "tar" ["-xf", xz1']) {cwd = Just tmpDir}) ""
    (out, ()) <- hCapture [stdout, stderr] $ cmdDupes defaultAppConfig d1'
    D.delay 100000 -- wait 0.1 second so we don't capture output from tasty
    return $ BLU.fromString out

test_demo_dupes :: TestTree
test_demo_dupes =
  let xz1 = "test/app/demo1.tar.xz"
      gld = "test/app/demo1.dupes"
  in goldenVsString
       "dupes demo1"
       gld
       (dupesTarXz xz1)
