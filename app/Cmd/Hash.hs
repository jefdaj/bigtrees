module Cmd.Hash where

-- TODO guess and check hashes

import Config (Config (..), defaultConfig)
import qualified Control.Concurrent.Thread.Delay as D
import Control.Exception (bracket)
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.List (isPrefixOf, sort)
import qualified System.Directory as SD
import System.Directory.BigTrees (buildProdTree, hWriteTree, printTree)
import qualified System.File.OsPath as SFO
import System.FilePath (dropExtension, takeBaseName, (<.>), (</>))
import System.Info (os)
import System.IO (Handle, IOMode (..), hClose, hFlush, openBinaryFile, stderr, stdout)
import System.IO.Silently (hCapture)
import System.IO.Temp (withSystemTempDirectory)
import System.OsPath (OsPath, encodeFS)
import System.Process (cwd, proc, readCreateProcess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)

--import Debug.Trace

cmdHash :: Config -> OsPath -> IO ()
cmdHash cfg path = bracket open close write
  where
    open = case txt cfg of
             Nothing -> return stdout
             Just p  -> SFO.openBinaryFile p WriteMode

    write hdl = do
      tree <- buildProdTree (verbose cfg) (exclude cfg) path
      hWriteTree (exclude cfg) hdl tree

    -- TODO why is this required? shouldn't hClose be OK?
    -- TODO maybe close it, but only if /= stdout?
    close = hFlush

-- The scan output includes current date + other metadata,
-- which we have to ignore if the tests are going to be reproducible.
-- The simplest thing for now is to remove all the comment lines.
-- TODO should the parser be robust to missing comments?
-- TODO if not, be careful not to re-use these for parsing tests
-- TODO or do a more specific list of regex substitutions (ew...)
stripComments :: String -> String
stripComments = unlines . filter (\l -> not ("#" `isPrefixOf` l)) . lines

hashTarXzAction :: Config -> FilePath -> IO BLU.ByteString
hashTarXzAction cfg xzPath = do
  withSystemTempDirectory "bigtrees" $ \tmpDir -> do
    let dPath = tmpDir </> dropExtension (takeBaseName xzPath) -- assumes .tar.something
    dPath' <- encodeFS dPath
    D.delay 100000 -- wait 0.1 second so we don't capture output from tasty
    _ <- readCreateProcess ((proc "tar" ["-xf", xzPath]) {cwd = Just tmpDir}) ""
    (out, ()) <- hCapture [stdout, stderr] $ cmdHash cfg dPath'
    D.delay 100000 -- wait 0.1 second so we don't capture output from tasty
    return $ BLU.fromString $ stripComments out

mkHashTarXzTest :: Config -> FilePath -> TestTree
mkHashTarXzTest cfg xzPath =
  -- TODO different extension since these aren't technically the same without comments?
  -- TODO something cleaner for os-specific golden files
  let gldPath = dropExtension (dropExtension xzPath) ++ "_" ++ os <.> "bigtree"
  in goldenVsString
       xzPath
       gldPath
       (hashTarXzAction cfg xzPath)

test_hash_tarxz :: IO TestTree
test_hash_tarxz = do
  xzPaths <- sort <$> findByExtension [".xz"] "test/app" -- TODO file bug about .tar.xz failing?
  xzPaths' <- mapM SD.makeAbsolute xzPaths
  -- putStrLn $ show xzPaths
  return $ testGroup
    "hash files extracted from tarballs"
    [mkHashTarXzTest defaultConfig p | p <- xzPaths']
