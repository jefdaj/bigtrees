module Cmd.Hash where

-- TODO guess and check hashes

import Config (Config (..), defaultConfig)
import qualified Control.Concurrent.Thread.Delay as D
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.List (sort)
import System.Directory.BigTrees
import System.Directory.BigTrees.FilePath (absolutize)
import System.FilePath (dropExtension, takeBaseName, (<.>), (</>))
import System.IO (stderr, stdout)
import System.IO.Silently (hCapture)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (cwd, proc, readCreateProcess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)

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
    Just p  -> writeBinForest p f

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
hashTarXzAction :: FilePath -> IO BLU.ByteString
hashTarXzAction xzPath = do
  (Just xzPath') <- absolutize xzPath
  withSystemTempDirectory "/tmp/bigtrees" $ \tmpDir -> do
    let dPath = tmpDir </> dropExtension (takeBaseName xzPath') -- assumes .tar.something
    D.delay 100000 -- wait 0.1 second so we don't capture output from tasty
    _ <- readCreateProcess ((proc "tar" ["-xf", xzPath']) {cwd = Just tmpDir}) ""
    (out, ()) <- hCapture [stdout, stderr] $ cmdHash defaultConfig [dPath]
    D.delay 100000 -- wait 0.1 second so we don't capture output from tasty
    return $ BLU.fromString out

mkHashTarXzTest :: FilePath -> TestTree
mkHashTarXzTest xzPath =
  let gldPath = dropExtension (dropExtension xzPath) <.> "bigtree"
  in goldenVsString
       xzPath
       gldPath
       (hashTarXzAction xzPath)

test_hash_tarxz :: IO TestTree
test_hash_tarxz = do
  xzPaths <- sort <$> findByExtension [".xz"] "test/app" -- TODO file bug about .tar.xz failing?
  -- putStrLn $ show xzPaths
  return $ testGroup
    "hash files extracted from tarballs"
    [mkHashTarXzTest p | p <- xzPaths]
