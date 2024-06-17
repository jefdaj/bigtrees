{-# LANGUAGE QuasiQuotes #-}

module Cmd.Find
  ( cmdFind
  , cmdFindUnixFind
  , prop_cmdFind_paths_match_unix_find
  )
  where

-- TODO use the actual path passed as the first breadcrumb? would match unix find

import Config (Config (..), defaultConfig)
import Control.Concurrent.Thread.Delay (delay)
import Data.List (sort)
import System.Directory.BigTrees (TestTree, listTreePaths, readOrBuildTree, writeTestTreeDir, treeName, unName)
import System.FilePath (takeBaseName, takeDirectory)
import System.IO (stderr, stdout)
import System.IO.Silently (hCapture)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (cwd, proc, readCreateProcess)
import Test.QuickCheck (Property, arbitrary)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)
-- import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import System.OsPath (OsPath, encodeFS, decodeFS, osp, (</>))
import qualified System.File.OsPath as SFO
import qualified System.Directory.OsPath as SDO
import qualified Data.ByteString.Char8 as B8

cmdFind :: Config -> OsPath -> IO ()
cmdFind cfg path = do
  tree <- readOrBuildTree (verbose cfg) (maxdepth cfg) (exclude cfg) path
  let paths = listTreePaths (regex cfg) (fromMaybe "" $ metafmt cfg) tree
  case txt cfg of
    Nothing -> mapM_ B8.putStrLn paths
    Just p  -> SFO.writeFile p $ B8.fromStrict $ B8.unlines paths -- TODO does this write lazily? we want it to

readAndSortLines :: OsPath -> IO B8.ByteString
readAndSortLines path = SFO.readFile' path >>= return . B8.unlines . sort . B8.lines

cmdFindUnixFind :: TestTree -> IO (B8.ByteString, B8.ByteString)
cmdFindUnixFind t =
  withSystemTempDirectory "bigtrees" $ \tmpDir -> do

    tmpDir' <- encodeFS tmpDir
    let treeDir'     = tmpDir' </> [osp|test-tree|]
    let myFindOut'   = tmpDir' </> [osp|my-find-output.txt|]
    let unixFindOut' = tmpDir' </> [osp|unix-find-output.txt|]
    unixFindOut <- decodeFS unixFindOut'

    -- treeDir' will be the *parent* of the root tree dir.
    -- we wrap it like this to make commands easier with potentially weird unicode tree names,
    -- and to avoid finding our own test txt files from above
    SDO.createDirectoryIfMissing False treeDir'
    writeTestTreeDir treeDir' t

    let cfg = defaultConfig { txt = Just myFindOut' }
    cmdFind cfg treeDir'

    -- Unix find will print whole absolute paths here, so we need to invoke it
    -- by relative path from the parent of the tmpdir to match my relative style.
    _ <- readCreateProcess ((proc "find" ["test-tree", "-fprint", unixFindOut]) {cwd = Just tmpDir}) ""

    out1 <- readAndSortLines myFindOut'
    out2 <- readAndSortLines unixFindOut'
    return (out1, out2)

prop_cmdFind_paths_match_unix_find :: Property
prop_cmdFind_paths_match_unix_find = monadicIO $ do
  tree <- pick arbitrary
  (out1, out2) <- run $ cmdFindUnixFind tree
  -- WARNING these will mess up your terminal
  -- liftIO $ putStrLn out1
  -- liftIO $ putStrLn out2
  assert $ out1 == out2
