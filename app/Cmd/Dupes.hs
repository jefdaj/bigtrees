module Cmd.Dupes where

-- TODO guess and check hashes

import Prelude hiding (log)
import Config (AppConfig (..), SearchConfig(..), defaultAppConfig, log)
import qualified Control.Concurrent.Thread.Delay as D
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified System.Directory as SD
import qualified System.Directory.BigTrees as BT
import qualified System.Directory.BigTrees (SearchConfig(..))
import System.FilePath (dropExtension, takeBaseName, (</>))
import System.IO.Silently (hCapture)
import System.IO.Temp (withSystemTempDirectory)
import System.OsPath (OsPath, encodeFS)
import System.Process (cwd, proc, readCreateProcess)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)
import qualified System.File.OsPath as SFO
import System.IO (Handle, IOMode (..), hClose, hFlush, openBinaryFile, stderr, stdout)
import Control.Exception (bracket)
import Control.Monad (forM)
import Control.Monad.ST.Strict (ST, runST)
import qualified Data.HashTable.Class as H

cmdDupes :: AppConfig -> OsPath -> IO ()
cmdDupes cfg path = bracket open close write
  where
    open = case outFile cfg of
             Nothing -> return stdout
             Just op -> SFO.openBinaryFile op WriteMode

    write hdl = do
      tree <- BT.readOrBuildTree (searchCfg cfg) (verbose cfg) path

      -- TODO move some of this to DupeMap? or is it better here?
      let rListPaths = referenceSetPaths $ searchCfg cfg
      log cfg rListPaths
      rList <- fmap concat $ forM rListPaths $ \fp -> encodeFS fp >>= BT.readHashList

      -- TODO should this all be one function exported from DupeMap?
      let ds = runST $ do
            mrSet <- if null rList
                       then return Nothing
                       else fmap Just $ BT.hashSetFromList rList
            -- ht <- BT.pathsByHash (searchCfg cfg) mrSet tree
            ht <- H.newSized $ maximum [length rList, 1000] -- TODO better defaults?
            BT.addTreeToDupeMap (searchCfg cfg) mrSet ht tree
            gs <- BT.scoreSets ht
            BT.dupesByNNodes ht

      BT.hWriteDupes (searchCfg cfg) hdl ds

    -- TODO why is this required? shouldn't hClose be OK?
    -- TODO maybe close it, but only if /= stdout?
    close = hFlush


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
