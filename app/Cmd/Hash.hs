module Cmd.Hash where

-- TODO guess and check hashes

import System.Directory.BigTrees
import Config (Config(..), log, defaultConfig)
-- import Run    (runGit, runGitCommit)

import Prelude hiding (log)

import Control.Monad    (when)
import Data.Maybe       (fromJust)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath  ((</>), (<.>), takeBaseName, dropExtension)

import Test.Tasty (TestTree, testGroup, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import qualified Control.Concurrent.Thread.Delay as D
import qualified Data.ByteString.Lazy.UTF8 as BLU
import System.IO (stdout, stderr)
import System.IO.Silently (hCapture)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readCreateProcess, proc, cwd)

-- the maybe filepath controls standalone (print hashes)
-- vs annex mode (write to the filepath)...
-- TODO is there a better way to set that up?
-- TODO require that the path be either absolute + in the annex or relative and in the annex
-- this works, but add doesn't. so what changed?
cmdHash :: Config -> [FilePath] -> IO ()
cmdHash cfg targets = do
  f <- buildForest (verbose cfg) (exclude cfg) targets
  case txt cfg of
    Nothing -> printForest f
    Just p  -> writeForest p f
  case bin cfg of
    Nothing -> return ()
    Just p -> writeBinForest p f

-- updateAnnexHashes :: Config -> HashTree () -> IO ()
-- updateAnnexHashes cfg new = do
--   let aPath   = fromJust $ annex cfg
--       hashes  = aPath </> "hashes.txt"
--       bHashes = aPath </> "hashes.bin"
--   log cfg "updating hashes.txt"
--   exists <- doesFileExist hashes
--   when exists $ do -- TODO only when verbose?
--     old <- readTree (maxdepth cfg) hashes
--     printDeltas $ diff old new -- just nice to verify this looks right
--   -- B.writeFile hashes $ serializeTree new
--   writeTree hashes new
--   -- TODO listen to config here? or always do it?
--   writeBinTree bHashes new
--   out1 <- runGit aPath ["add", "hashes.txt"]
--   log cfg out1
--   out2 <- runGit aPath ["add", "hashes.bin"]
--   log cfg out2

guardStatus :: Config -> FilePath -> IO ()
guardStatus = undefined
  -- TODO check that git status is all clear
  -- TODO check that git-annex status is all clear too?

guardHash :: Config -> Maybe FilePath -> FilePath -> IO ()
guardHash = undefined
  -- TODO run guardInit here?
  -- TODO run guardStatus here?
  -- TODO check that hashes.txt exists

-----------
-- tests --
-----------

-- TODO random names? or all one bigtrees dir? or prefixed as it is?
untar_and_hash_to_bs :: FilePath -> IO BLU.ByteString
untar_and_hash_to_bs xzPath = do
  wd <- getCurrentDirectory
  let xzPath' = wd </> xzPath
  withSystemTempDirectory "/tmp/bigtrees" $ \tmpDir -> do
    let dPath = tmpDir </> dropExtension (takeBaseName xzPath')
    D.delay 100000 -- wait 0.1 second so we don't capture output from tasty
    _ <- readCreateProcess ((proc "tar" ["-xf", xzPath']) {cwd = Just tmpDir}) ""
    (out, ()) <- hCapture [stdout, stderr] $ cmdHash defaultConfig [dPath]
    D.delay 100000 -- wait 0.1 second so we don't capture output from tasty
    return $ BLU.fromString out

test_hash_demo1_dir :: TestTree
test_hash_demo1_dir = testGroup "hash tests" [testAction]
  where
    dirPath = "test/app/demo1.tar.xz"
    gldPath = "test/app/demo1.golden" -- TODO .txt?
    testAction = goldenVsString
      "hash demo1 dir"
      gldPath
      (untar_and_hash_to_bs dirPath)
