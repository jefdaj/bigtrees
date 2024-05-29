module Cmd.Hash where

-- TODO guess and check hashes

import Control.DeepSeq (deepseq, force)
import Control.Exception (evaluate)
import Config (Config (..), defaultConfig)
import qualified Control.Concurrent.Thread.Delay as D
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.List (sort)
import System.Directory.BigTrees (buildProdTree, printTree, hWriteTree)
import System.Directory.BigTrees.HeadFoot (hWriteHeader, hWriteFooter)
import System.Directory.BigTrees.Util (absolutePath)
import System.FilePath (dropExtension, takeBaseName, (<.>), (</>))
import System.IO (stderr, stdout, openBinaryFile, IOMode(..), Handle, hClose)
import System.IO.Silently (hCapture)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (cwd, proc, readCreateProcess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Control.Exception (bracket) -- TODO .Safe?

cmdHash :: Config -> FilePath -> IO ()
cmdHash cfg path = bracket open close write
  where
    open = case txt cfg of
             Nothing -> return stdout
             Just p  -> openBinaryFile p WriteMode
    close = hClose
    write hdl = do
      -- TODO it should be simple at least to force the header to print, right?? lol
      --      why doesn't even the putStrLn run?
      putStrLn "start write"
      hWriteHeader hdl (exclude cfg)
      hWriteTree hdl =<< buildProdTree (verbose cfg) (exclude cfg) path
      hWriteFooter hdl

hashTarXzAction :: FilePath -> IO BLU.ByteString
hashTarXzAction xzPath = do
  (Just xzPath') <- absolutePath xzPath
  withSystemTempDirectory "/tmp/bigtrees" $ \tmpDir -> do
    let dPath = tmpDir </> dropExtension (takeBaseName xzPath') -- assumes .tar.something
    D.delay 100000 -- wait 0.1 second so we don't capture output from tasty
    _ <- readCreateProcess ((proc "tar" ["-xf", xzPath']) {cwd = Just tmpDir}) ""
    (out, ()) <- hCapture [stdout, stderr] $ cmdHash defaultConfig dPath
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
