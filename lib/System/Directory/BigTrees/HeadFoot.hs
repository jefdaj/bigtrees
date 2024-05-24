module System.Directory.BigTrees.HeadFoot where

import System.Info (os, arch, compilerName, fullCompilerVersion)
import Data.Version (showVersion)
import System.Environment (getEnv, getProgName)
import Paths_bigtrees (version)
import System.Directory.BigTrees.HashLine (hashLineFields)
import System.FilePath.Glob (Pattern)
import Data.Time.Clock.POSIX (getPOSIXTime)
-- import Data.Time.Clock (secondsToDiffTime)
import qualified Data.ByteString.Char8 as B8
import System.IO (Handle)

{- Header + footer info to write before and after HashLines, respectively.
 - The initial format is to read/write JSON delimited from other lines by '#'.
 -
 - These are/will be used for:
 -
 - * Printing general info about a tree file
 - * Calculating how long a scan took
 - * Calculating whether a tree file is newer than the file(s) it refers to
 - * Upgrading old versions of tree files as the format evolves
 -}

-- TODO generalize so we can fit other things in the header too? or are explicit values enough?

data Header = Header
  { excludes  :: [Pattern]
  , maxdepth  :: Maybe Int
  , system    :: (String, String) -- os, arch
  , compiler  :: (String, String) -- compiler, version
  , bigtrees  :: String           -- format version string, from cabal file
  , scanStart :: Integer
  , fields    :: [String]         -- field order, hardcoded
  }
  deriving (Show, Read)

now :: IO Integer
now = getPOSIXTime >>= return . round

makeHeaderNow :: [Pattern] -> Maybe Int -> IO Header
makeHeaderNow es md = do
  progName  <- getProgName
  startTime <- now
  let header = Header
        { excludes  = es
        , maxdepth  = md
        , system    = (os, arch)
        , compiler  = (compilerName, showVersion fullCompilerVersion)
        , bigtrees  = showVersion version
        , scanStart = startTime
        , fields    = hashLineFields
        }
  return header

data Footer = Footer
  { scanEnd    :: Integer
  , nSuccesses :: Int -- TODO integer?
  , nErrors    :: Int
  }
  deriving (Show, Read)

makeFooterNow :: (Int, Int) -> IO Footer
makeFooterNow (nOK, nErr) = do
  endTime <- now
  let footer = Footer
        { scanEnd    = endTime
        , nSuccesses = nOK
        , nErrors    = nErr
        }
  return footer

-- TODO proper time type for this?
scanSeconds :: (Header, Footer) -> Integer
scanSeconds (h, f) = scanEnd f - scanStart h

treeInfo :: (Header, Footer) -> B8.ByteString
treeInfo = undefined

writeHeader :: Handle -> IO ()
writeHeader h = do
  -- TODO bigtrees: version
  -- TODO system: os, arch, locale
  -- TODO config: maxdepth, excludes
  -- TODO scan: start time
  -- TODO is filesystem easy?
  -- hPrintf h $ ""
  return ()

writeFooter :: Handle -> IO ()
writeFooter h = do
  -- TODO n errors?
  -- TODO end time
  return ()
