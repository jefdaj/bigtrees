{-# LANGUAGE OverloadedStrings #-}

module Cmd.Dupes where

-- TODO guess and check hashes

import Cmd.Dupes.Render (DupesRenderFn, dupesRenderFunctions)
import Config (AppConfig (..), SearchConfig (..), defaultAppConfig)
import qualified Control.Concurrent.Thread.Delay as D
import Control.Exception (bracket)
import Control.DeepSeq (force, deepseq)
import Control.Monad (forM, (>=>))
import Control.Monad.ST.Strict (ST, runST)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.HashTable.Class as H
import Data.Maybe (fromJust, fromMaybe)
import Prelude hiding (log)
import qualified System.Directory as SD
import qualified System.Directory.BigTrees as BT
import System.Directory.BigTrees.Logging (LogCfg (..), LogLevel (..), addLogContext, log, logUnsafe)
import qualified System.File.OsPath as SFO
import System.FilePath (dropExtension, takeBaseName, (</>))
import System.IO (Handle, IOMode (..), hClose, hFlush, openBinaryFile, stderr, stdout)
import System.IO.Silently (hCapture)
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.OsPath (OsPath, encodeFS)
import System.Process (cwd, proc, readCreateProcess)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)
import Control.DeepSeq (deepseq)

-- import Debug.Trace

-- defined in DupeMap.hs for now:
-- TODO rename DupesRenderFn
-- type DupesRenderFn = Maybe Depth -> SortedDupeLists -> IO B8.ByteString

hWriteDupes :: SearchConfig -> LogCfg -> DupesRenderFn -> Bool -> Handle -> BT.SortedDupeLists -> IO ()
hWriteDupes cfg lCfg explainFn noRefSet hdl groups = do
  -- TODO rename line groups or similar?
  let debug = log (addLogContext lCfg "hWriteDupes") DebugL
  lines <- map (\x -> deepseq x x) <$> explainFn lCfg noRefSet (maxDepth cfg) groups
  debug "writing dupes to output handle"
  mapM_ (\l -> B8.hPutStrLn hdl l >> hFlush hdl) lines -- TODO this will force evaluation line by line, right?

cmdDupes :: AppConfig -> LogCfg -> OsPath -> IO ()
cmdDupes cfg lCfg path = bracket open close write
  where

    lCfg' = (addLogContext lCfg "cmdDupes")
    debug = log lCfg' DebugL
    debugST msg = logUnsafe lCfg' DebugL msg (return ())

    open = case outFile cfg of
             Nothing -> return stdout
             Just op -> SFO.openBinaryFile op WriteMode

    write :: Handle -> IO ()
    write hdl = do

      let searches = dupesExcludeSearches $ searchCfg cfg
      debug $ "compiling " <> B8.pack (show $ length searches) <> " labeled searches "
      cle <- BT.compileLabeledSearches searches

      -- TODO move some of this to DupeMap?
      let rListPaths = referenceSetPaths $ searchCfg cfg
          noRefSet = null rListPaths
      debug $ "loading reference sets " <> B8.pack (show rListPaths)
      rList <- fmap concat $ forM rListPaths (encodeFS >=> BT.readHashList lCfg')
      debug $ "loaded " <> B8.pack (show $ length rList) <> " reference hashes"

      tree <- BT.readOrBuildTree (searchCfg cfg) lCfg' path

      -- TODO should this all be one function exported from DupeMap?
      -- TODO these debugST calls do *NOT* work at the right times; replace
      let ds = runST $ do
            debugST "runST starting"

            -- normally, we want to be sure not to delete all copies of a file!
            -- but in the special case of dupes vs a reference set, it should be ok
            -- TODO any good way to warn the user if their ref set looks like it's inside the dupes?
            mrSet <- if noRefSet
                       then debugST "no ref sets" >> return Nothing
                       else Just <$> BT.hashSetFromList rList

            let init  = max (length mrSet) 1000 -- TODO better defaults?
                initB = B8.pack $ show init
                treeN = B8.pack $ show $ (\(BT.NNodes n ) -> n) $ BT.treeNNodes tree
 

            debugST $ "creating DupeMap sized " <> initB
            ht <- H.newSized init
            -- nBefore <- BT.dmSize ht
	    BT.addTreeToDupeMap (searchCfg cfg) lCfg' mrSet cle ht tree
            -- nAfter <- BT.dmSize ht
            -- debugST $ "hashtable size " <> B8.pack (show nBefore) <> " -> " <> B8.pack (show nAfter)
            debugST $ "added all " <> treeN <> " tree nodes to DupeMap"
            if noRefSet then debugST "scoring dupes" else debugST "scoring dupes vs reference set"
            -- TODO DupesMode or similar type to make the null rList thing more obvious?
            let scoreFn = if noRefSet then BT.scoreSetSelf else BT.scoreSetRef
                singlesAreDupes = not noRefSet
            res <- map force <$> BT.dupesByNegScore lCfg' scoreFn singlesAreDupes ht
            -- TODO does this print before it starts actually scoring sets?
            debugST $ "finished scoring " <> treeN <> " DupeSets"
            return res

      -- TODO pull default from docopt instead of duplicating that here
      let fmt = fromMaybe "suggestions" $ dupesOutFormat cfg

      let renderFn = fromJust $ lookup fmt dupesRenderFunctions

      -- TODO does this help force the overall evaluation order?
      -- TODO try force instead?
      let ds' = rList `deepseq` ds

      debug $ "writing " <> B8.pack (show $ length ds') <> " DupeSets"
      hWriteDupes (searchCfg cfg) lCfg' renderFn noRefSet hdl ds'

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
