{-# LANGUAGE OverloadedStrings #-}

module Cmd.Dupes where

-- TODO guess and check hashes

import Prelude hiding (log)
import Config (AppConfig (..), SearchConfig(..), defaultAppConfig)
import qualified Control.Concurrent.Thread.Delay as D
import Control.Exception (bracket)
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified System.Directory as SD
import qualified System.Directory.BigTrees as BT
import System.FilePath (dropExtension, takeBaseName, (</>))
import System.IO (Handle, IOMode (..), hClose, hFlush, openBinaryFile, stderr, stdout)
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
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.ByteString.Char8 as B8
import System.IO.Unsafe (unsafePerformIO)
import System.Directory.BigTrees.Logging (LogCfg (..), LogLevel (..), log, logUnsafe, addLogContext)

-- import Debug.Trace

-- defined in DupeMap.hs for now:
-- TODO rename DupesRenderFn
-- type ExplainFn = Maybe Depth -> SortedDupeLists -> IO B8.ByteString

dupesRenderFunctions :: [(String, BT.ExplainFn)]
dupesRenderFunctions =
  [ ("suggestions", BT.renderDupesSuggestions)
  , ("rsync-filter-file", BT.renderDupesRsyncFilter)
  ]

cmdDupes :: AppConfig -> LogCfg -> OsPath -> IO ()
cmdDupes cfg lCfg path = bracket open close write
  where

    debug = log (addLogContext lCfg "cmdDupes") DebugL
    debugST msg = logUnsafe (addLogContext lCfg "cmdDupes") DebugL msg (return ())

    open = case outFile cfg of
             Nothing -> return stdout
             Just op -> SFO.openBinaryFile op WriteMode

    write hdl = do
      tree <- BT.readOrBuildTree (searchCfg cfg) lCfg path

      -- TODO move some of this to DupeMap?
      let rListPaths = referenceSetPaths $ searchCfg cfg
      debug $ "loading rList from " <> B8.pack (show (length rListPaths)) <> " paths"
      rList <- fmap concat $ forM rListPaths $ \fp -> encodeFS fp >>= BT.readHashList lCfg

      debug "compiling labeled searches"
      cle <- BT.compileLabeledSearches $ dupesExcludeSearches $ searchCfg cfg

      -- TODO should this all be one function exported from DupeMap?
      let ds = runST $ do
	    -- TODO move this inside addTreeToDupeMap progRef <- newSTRef 0
	    debugST "runST starting"
            mrSet <- if null rList
                       then return Nothing
                       else fmap Just $ BT.hashSetFromList rList
            let init  = maximum [length mrSet, 1000] -- TODO better defaults?
                initB = B8.pack $ show init
            debugST $ "creating DupeMap sized " <> initB
            ht <- H.newSized init
            BT.addTreeToDupeMap (searchCfg cfg) lCfg mrSet cle ht tree
	    -- debugST $ "added all " <> initB <> " tree nodes to DupeMap"
	    if null rList then debugST "scoring dupes" else debugST "scoring dupes vs reference set"
            let scoreFn = if null rList then BT.scoreSetSelf else BT.scoreSetRef
            res <- BT.dupesByNegScore lCfg scoreFn ht
	    -- debugST $ "finished scoring " <> initB <> " DupeSets" -- TODO but is this time ordered?
            return res

      -- TODO pull default from docopt instead of duplicating that here
      let fmt = fromMaybe "comments" $ dupesOutFormat cfg

      let renderFn = fromJust $ lookup fmt dupesRenderFunctions

      -- normally, we want to be sure not to delete all copies of a file!
      -- but in the special case of dupes vs a reference set, it should be ok
      -- TODO any good way to warn the user if their ref set looks like it's inside the dupes?
      let keepOneDupe = null rList

      debug $ "writing " <> B8.pack (show $ length ds) <> " DupeSets"
      BT.hWriteDupes (searchCfg cfg) renderFn keepOneDupe hdl ds

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
    (out, ()) <- hCapture [stdout, stderr] $ cmdDupes defaultAppConfig NoLog d1'
    D.delay 100000 -- wait 0.1 second so we don't capture output from tasty
    return $ BLU.fromString out

test_demo_dupes :: TestTree
test_demo_dupes =
  let xz1 = "test/app/demo1.tar.xz"
      gld = "test/app/demo1.bigdupes"
  in goldenVsString
       "dupes demo1"
       gld
       (dupesTarXz xz1)
