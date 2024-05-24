module System.Directory.BigTrees.HeadFoot where

import System.Info (os, arch, compilerName, fullCompilerVersion)
import Data.Version (showVersion)
import System.Environment (getEnv, getProgName)
import Paths_bigtrees (version)
import System.Directory.BigTrees.HashLine (hashLineFields)
import System.FilePath.Glob (Pattern)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime, utcTimeToPOSIXSeconds)
import Data.Time.Clock (secondsToDiffTime)

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
  , scanStart :: POSIXTime
  , fields    :: [String]         -- field order, hardcoded
  }

now :: IO Integer
now = getPOSIXTime >>= return . round . utcTimeToPOSIXSeconds

makeHeader :: [Pattern] -> Maybe Int -> IO Header
makeHeader es md = do
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
  { scanEnd    :: POSIXTime
  , nSuccesses :: Int -- TODO integer?
  , nErrors    :: Int
  }

makeFooter :: (Int, Int) -> IO Footer
makeFooter nOK nErr = do
  endTime <- now
  let footer = Footer
        { scanEnd    = endTime
        , nSuccesses = nOK
        , nErrors    = nErr
        }
  return footer

-- TODO proper time type for this?
scanDuration :: (Header a, Footer) -> String
scanDuration = undefined

treeInfo :: (Header, Footer) -> B8.ByteString
treeInfo = undefined
