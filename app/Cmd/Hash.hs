module Cmd.Hash where

-- TODO guess and check hashes

import Config (Config (..), defaultConfig)
import qualified Control.Concurrent.Thread.Delay as D
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.List (sort)
import System.Directory.BigTrees (buildForest, printForest, writeForest)
import System.Directory.BigTrees.Util (absolutePath)
import System.FilePath (dropExtension, takeBaseName, (<.>), (</>))
import System.IO (stderr, stdout)
import System.IO.Silently (hCapture)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (cwd, proc, readCreateProcess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)

cmdHash :: Config -> [FilePath] -> IO ()
cmdHash cfg targets = do
  f <- buildForest (verbose cfg) (exclude cfg) targets
  case txt cfg of
    Nothing -> printForest f
    Just p  -> writeForest p f

hashTarXzAction :: FilePath -> IO BLU.ByteString
hashTarXzAction xzPath = do
  (Just xzPath') <- absolutePath xzPath
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
