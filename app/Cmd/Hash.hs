module Cmd.Hash where

-- TODO guess and check hashes

import Config (Config (..), defaultConfig)
import qualified Control.Concurrent.Thread.Delay as D
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.List (sort, isPrefixOf)
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
import System.Info (os)

cmdHash :: Config -> FilePath -> IO ()
cmdHash cfg path = bracket open close write
  where
    open = case txt cfg of
             Nothing -> return stdout
             Just p  -> openBinaryFile p WriteMode

    -- TODO why is this required? shouldn't hClose be OK?
    -- TODO maybe close it, but only if /= stdout?
    close = \_ -> return ()

    write hdl = do
      hWriteHeader hdl (exclude cfg)
      tree <- buildProdTree (verbose cfg) (exclude cfg) path
      hWriteTree hdl tree
      hWriteFooter hdl

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
  (Just xzPath') <- absolutePath xzPath
  withSystemTempDirectory "bigtrees" $ \tmpDir -> do
    let dPath = tmpDir </> dropExtension (takeBaseName xzPath') -- assumes .tar.something
    D.delay 100000 -- wait 0.1 second so we don't capture output from tasty
    _ <- readCreateProcess ((proc "tar" ["-xf", xzPath']) {cwd = Just tmpDir}) ""
    (out, ()) <- hCapture [stdout, stderr] $ cmdHash cfg dPath
    D.delay 100000 -- wait 0.1 second so we don't capture output from tasty
    return $ BLU.fromString $ stripComments out

mkHashTarXzTest :: Config -> String -> FilePath -> TestTree
mkHashTarXzTest cfg prefix xzPath =
  -- TODO different extension since these aren't technically the same without comments?
  -- TODO something cleaner for os-specific golden files
  let gldPath = dropExtension (dropExtension xzPath) ++ "_" ++ prefix ++ "_" ++ os <.> "bigtree"
  in goldenVsString
       xzPath
       gldPath
       (hashTarXzAction cfg xzPath)

test_hash_tarxz :: IO TestTree
test_hash_tarxz = do
  xzPaths <- sort <$> findByExtension [".xz"] "test/app" -- TODO file bug about .tar.xz failing?
  -- putStrLn $ show xzPaths
  return $ testGroup
    "hash files extracted from tarballs"
    [mkHashTarXzTest defaultConfig "default" p | p <- xzPaths]

test_hash_annex1_git_dir :: IO TestTree
test_hash_annex1_git_dir = do
  let xzPath = "test/app/annex1.tar.xz"
      cfg = defaultConfig { exclude = [] } -- disable exclude ptns
  return $ testGroup
    "hash annex1 including the .git dir"
    [mkHashTarXzTest cfg "noexclude" xzPath]
