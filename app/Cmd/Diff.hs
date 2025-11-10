{-# LANGUAGE QuasiQuotes #-}

module Cmd.Diff
  ( cmdDiff
  , diffTarXz
  , test_demo_diff
  )
  where

import Config (AppConfig (..), defaultAppConfig)
import qualified Control.Concurrent.Thread.Delay as D
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified System.Directory as SD
import System.Directory.BigTrees (Name (..), SearchConfig (..), diff, printDeltas, writeDeltas, readOrBuildTree,
                                  renameRoot)
import System.Directory.BigTrees.Logging (LogCfg (..))
import System.FilePath (dropExtension, takeBaseName, (</>))
import System.IO (stderr, stdout)
import System.IO.Silently (hCapture)
import System.IO.Temp (withSystemTempDirectory)
import System.OsPath (OsPath, encodeFS, osp)
import System.Process (cwd, proc, readCreateProcess)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)

cmdDiff :: AppConfig -> LogCfg -> OsPath -> OsPath -> IO ()
cmdDiff cfg lCfg old new = do
  old <- renameRoot (Name [osp|old|]) <$> readOrBuildTree (searchCfg cfg) lCfg old
  new <- renameRoot (Name [osp|new|]) <$> readOrBuildTree (searchCfg cfg) lCfg new
  let deltas = diff lCfg old new
  case outFile cfg of
    Nothing -> printDeltas deltas
    Just p  -> writeDeltas p deltas

-----------
-- tests --
-----------

diffTarXz :: FilePath -> FilePath -> IO BLU.ByteString
diffTarXz xz1 xz2 = do
  xz1' <- SD.makeAbsolute xz1
  xz2' <- SD.makeAbsolute xz2
  withSystemTempDirectory "bigtrees" $ \tmpDir -> do
    let d1 = tmpDir </> dropExtension (takeBaseName xz1)
    let d2 = tmpDir </> dropExtension (takeBaseName xz2)
    d1' <- encodeFS d1
    d2' <- encodeFS d2
    D.delay 100000 -- wait 0.1 second so we don't capture output from tasty
    _ <- readCreateProcess ((proc "tar" ["-xf", xz1']) {cwd = Just tmpDir}) ""
    _ <- readCreateProcess ((proc "tar" ["-xf", xz2']) {cwd = Just tmpDir}) ""
    (out, ()) <- hCapture [stdout, stderr] $ cmdDiff defaultAppConfig NoLog d1' d2'
    D.delay 100000 -- wait 0.1 second so we don't capture output from tasty
    return $ BLU.fromString out

test_demo_diff :: TestTree
test_demo_diff =
  let xz1 = "test/app/demo1.tar.xz"
      xz2 = "test/app/demo2.tar.xz"
      gld = "test/app/demo12.bigdiff"
  in goldenVsString
       "diff demo1 -> demo2"
       gld
       (diffTarXz xz1 xz2)
