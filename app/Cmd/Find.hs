{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Cmd.Find
  ( cmdFind
  , cmdFindUnixFind
  , prop_cmdFind_paths_match_unix_find
  )
  where

-- TODO use the actual path passed as the first breadcrumb? would match unix find

import Prelude hiding (log)
import Config (AppConfig (..), defaultAppConfig)
-- import Control.Concurrent.Thread.Delay (delay)
import Data.List (sort)
import System.Directory.BigTrees (TestTree, listTreePaths, readOrBuildTree, treeName, unName,
                                  writeTestTreeDir)
-- import System.FilePath (takeBaseName, takeDirectory)
-- import System.IO (stderr, stdout)
-- import System.IO.Silently (hCapture)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (cwd, proc, readCreateProcess)
import Test.QuickCheck (Property)
-- import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)
-- import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B8
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import System.Directory.BigTrees.Logging (LogCfg (..), LogLevel (..), addLogContext, log)
import System.Directory.BigTrees.Util (propertyWithExceptions)
import qualified System.Directory.OsPath as SDO
import qualified System.File.OsPath as SFO
import System.OsPath (OsPath, decodeFS, encodeFS, osp, (</>))

cmdFind :: AppConfig -> LogCfg -> OsPath -> IO ()
cmdFind cfg lCfg path = do
  let lCfg' = addLogContext lCfg "cmdFind"
      debug = log lCfg' DebugL

  debug "reading tree"
  tree <- readOrBuildTree (searchCfg cfg) lCfg path
  debug "done reading tree"


  -- I think hashes have to be removed here rather than above in the read/build
  -- step (when building, not reading), because we don't want to alter the dir hashes.
  -- TODO should the exclude regexes also not be done at first? Think about pros/cons
  -- TODO is this a reason to separate read from build more definitively?
  debug "listing paths"
  let fmt = fromMaybe "" $ findOutFormat cfg
  paths <- listTreePaths (searchCfg cfg) lCfg' fmt tree
  debug "done listing paths"

  case outFile cfg of
    Nothing -> do
      -- TODO avoid forcing with length here?
      debug $ "writing " <> B8.pack (show $ length paths) <> " paths to file"
      mapM_ B8.putStrLn paths
    Just p  -> do
      -- TODO avoid forcing with length here?
      debug $ "writing " <> B8.pack (show $ length paths) <> " paths to file"
      SFO.writeFile p $ B8.fromStrict $ B8.unlines paths

readAndSortLines :: OsPath -> IO B8.ByteString
readAndSortLines path = SFO.readFile' path <&> (B8.unlines . sort . B8.lines)

cmdFindUnixFind :: LogCfg -> TestTree -> IO (B8.ByteString, B8.ByteString)
cmdFindUnixFind lCfg t =
  withSystemTempDirectory "bigtrees" $ \osTmpDir -> do
    tmpDir <- encodeFS osTmpDir

    let testDir    = tmpDir </> [osp|test-tree|]
        myOutput   = tmpDir </> [osp|my-find-output.txt|]
        unixOutput = tmpDir </> [osp|unix-find-output.txt|]

    -- write the tree to a root inside the test-tree dir
    -- TODO would a retry here prevent occasional failures? or is that something else?
    SDO.createDirectoryIfMissing True testDir
    let treeRootDir = testDir </> unName (treeName t)
    writeTestTreeDir lCfg treeRootDir t

    -- find tree paths and write them to my-find-output.txt
    let cfg = defaultAppConfig { outFile = Just myOutput }
    cmdFind cfg NoLog testDir

    -- Unix find tree paths and write them to unix-find-output.txt
    -- Unix find will print whole absolute paths here, so we need to invoke it
    -- by relative path from the parent of the tmpdir to match my relative style.
    -- TODO extra test-tree wrapper dir prevents encoding errors in the command line args?
    osUnixOutput <- decodeFS unixOutput
    _ <- flip readCreateProcess "" $
           (proc "find" ["test-tree", "-fprint", osUnixOutput])
           {cwd = Just osTmpDir}

    -- return both versions for comparison
    -- TODO would comparing them directly here make more sense?
    out1 <- readAndSortLines myOutput
    out2 <- readAndSortLines unixOutput
    return (out1, out2)

prop_cmdFind_paths_match_unix_find :: Property
prop_cmdFind_paths_match_unix_find = propertyWithExceptions
  (cmdFindUnixFind NoLog)
  (\_ (a,b) -> a == b) -- takes input and output, compares fst and snd of output
