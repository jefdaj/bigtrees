{-# LANGUAGE DeriveGeneric #-}

module System.Directory.BigTrees.HeadFoot where

import System.Info (os, arch, compilerName, fullCompilerVersion)
import Data.Version (showVersion)
import System.Environment (getEnv, getProgName)
import Paths_bigtrees (version)
import System.Directory.BigTrees.HashLine (hashLineFields, join)
-- import System.FilePath.Glob (Pattern)
import Data.Time.Clock.POSIX (getPOSIXTime)
-- import Data.Time.Clock (secondsToDiffTime)
import qualified Data.ByteString.Char8 as B8
import System.IO (Handle)
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

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
  { excludes  :: [String]
  , maxdepth  :: Maybe Int
  , system    :: (String, String) -- os, arch
  , compiler  :: (String, String) -- compiler, version
  , bigtrees  :: String           -- format version string, from cabal file
  , scanStart :: Integer
  , fields    :: [String]         -- field order, hardcoded
  }
  deriving (Eq, Read, Show, Generic)

instance ToJSON   Header
instance FromJSON Header

now :: IO Integer
now = getPOSIXTime >>= return . round

makeHeaderNow :: [String] -> Maybe Int -> IO Header
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

headerPrefix :: B8.ByteString
headerPrefix = "# "

renderHeader :: Header -> B8.ByteString
renderHeader h = undefined -- rmFields (toJSON h) ++ fields h

data Footer = Footer
  { scanEnd    :: Integer
  , nSuccesses :: Int -- TODO integer?
  , nErrors    :: Int
  }
  deriving (Eq, Read, Show, Generic)

instance ToJSON   Footer
instance FromJSON Footer

makeFooterNow :: (Int, Int) -> IO Footer
makeFooterNow (nOK, nErr) = do
  endTime <- now
  let footer = Footer
        { scanEnd    = endTime
        , nSuccesses = nOK
        , nErrors    = nErr
        }
  return footer

renderFooter :: Footer -> B8.ByteString
renderFooter f = undefined

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
