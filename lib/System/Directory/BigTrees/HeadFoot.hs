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
import qualified Data.Aeson.Encode.Pretty as AP
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

--- utilities ---

apConf :: AP.Config
apConf = AP.defConfig
  { AP.confIndent = AP.Spaces 2
  , AP.confTrailingNewline = True
  }

now :: IO Integer
now = getPOSIXTime >>= return . round

commentLines :: [B8.ByteString] -> [B8.ByteString]
commentLines = map (B8.append "# ")

-- TODO proper time type for this?
scanSeconds :: (Header, Footer) -> Integer
scanSeconds (h, f) = scanEnd f - scanStart h

treeInfo :: (Header, Footer) -> B8.ByteString
treeInfo = undefined

--- header ---

data Header = Header
  { excludes  :: [String]
  , maxdepth  :: Maybe Int
  , system    :: String -- os, arch TODO uname?
  , compiler  :: String -- compiler, version
  , bigtrees  :: String -- format version string, from cabal file
  , scanFormat :: Int
  , scanStart :: Integer
  -- TODO locale
  }
  deriving (Eq, Read, Show, Generic)

instance ToJSON   Header
instance FromJSON Header

makeHeaderNow :: [String] -> Maybe Int -> IO Header
makeHeaderNow es md = do
  progName  <- getProgName
  startTime <- now
  let header = Header
        { excludes  = es
        , maxdepth  = md
        , system    = unwords [os, arch]
        , compiler  = unwords [compilerName, showVersion fullCompilerVersion]
        , bigtrees  = showVersion version
        , scanFormat = 2 -- update when changing anything that breaks parser
        , scanStart = startTime
        }
  return header

renderHeader :: Header -> B8.ByteString
renderHeader h = B8.unlines $ commentLines $ (B8.lines header) ++ [fields]
  where
    header = B8.toStrict $ AP.encodePretty' apConf h
    fields = join $ map B8.pack hashLineFields

writeHeader :: Handle -> [String] -> Maybe Int -> IO ()
writeHeader hdl es md = do
  hdr <- makeHeaderNow es md
  B8.hPutStrLn hdl $ renderHeader hdr

--- footer ---

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
renderFooter f = B8.unlines $ commentLines $ B8.lines footer
  where
    footer = B8.toStrict $ AP.encodePretty' apConf f

writeFooter :: Handle -> (Int, Int) -> IO ()
writeFooter hdl oe = do
  ftr <- makeFooterNow oe
  B8.hPutStrLn hdl $ renderFooter ftr
