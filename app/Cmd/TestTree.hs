module Cmd.TestTree where

-- TODO guess and check hashes

import Config (AppConfig (..), defaultAppConfig)
import qualified Control.Concurrent.Thread.Delay as D
import Control.Exception (bracket)
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.List (isPrefixOf, sort)
import qualified System.Directory as SD
import System.Directory.BigTrees (buildProdTree, hWriteTree, printTree, listTreePaths, readOrBuildTree)
import Data.Maybe (fromMaybe)
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
import System.Directory.BigTrees.Logging (LogCfg (..), LogLevel (..), log, logUnsafe, addLogContext)

--import Debug.Trace

-- TODO also test that there aren't others being missed? no, diff handles that

cmdTestTree :: AppConfig -> LogCfg -> OsPath -> OsPath -> IO ()
cmdTestTree cfg lCfg treePath dirPath = do
  tree <- readOrBuildTree (searchCfg cfg) lCfg treePath
  let fmt = fromMaybe "" $ findOutFormat cfg

  -- TODO also test the tree itself in case the paths differ from the data structure?
  paths <- listTreePaths (searchCfg cfg) lCfg fmt tree

  -- TODO for each path, test whether it exists

  return ()
