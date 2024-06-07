{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Directory.BigTrees.HeadFoot where

import Data.Version (showVersion)
import Paths_bigtrees (version)
import System.Directory.BigTrees.HashLine (hashLineFields, join)
import System.Environment (getEnv, getProgName)
import System.Info (arch, compilerName, fullCompilerVersion, os)
-- import System.FilePath.Glob (Pattern)
import Data.Time.Clock.POSIX (getPOSIXTime)
-- import Data.Time.Clock (secondsToDiffTime)
import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Char8 as B8
import Data.Functor ((<&>))
import GHC.Generics (Generic)
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

--- utilities ---

apConf :: AP.Config
apConf = AP.defConfig
  { AP.confIndent = AP.Spaces 2
  , AP.confTrailingNewline = True
  }

now :: IO Integer
now = getPOSIXTime <&> round

commentLines :: [B8.ByteString] -> [B8.ByteString]
commentLines = map (B8.append "# ")

-- TODO proper time type for this?
scanSeconds :: (Header, Footer) -> Integer
scanSeconds (h, f) = scanEnd f - scanStart h

treeInfo :: (Header, Footer) -> B8.ByteString
treeInfo = undefined

--- header ---

data Header = Header
  { compiler        :: String -- compiler, version
  , excludePatterns :: [String]
  , locale          :: String
  , program         :: String -- format version string, from cabal file
  , scanStart       :: Integer
  , system          :: String -- os, arch TODO uname?
  , treeFormat      :: Int
  }
  deriving (Eq, Read, Show, Generic)

instance ToJSON   Header
instance FromJSON Header

-- update when changing anything that might break the parser, and keep track of
-- prev versions with git tags in order to implement upgrade fns later
currentTreeFormat :: Int
currentTreeFormat = 240529

makeHeaderNow :: [String] -> IO Header
makeHeaderNow es = do
  progName  <- getProgName
  scanStart <- now
  lang      <- getEnv "LANG" -- TODO locale? LC_ALL? others?
  let header = Header
        { compiler        = unwords [compilerName, showVersion fullCompilerVersion]
        , excludePatterns = es
        , locale          = lang
        , program         = unwords [progName, showVersion version]
        , scanStart       = scanStart
        , system          = arch ++ "-" ++ os
        , treeFormat      = currentTreeFormat
        }
  return header

renderHeader :: Header -> B8.ByteString
renderHeader h = B8.unlines $ commentLines $ (B8.lines header) ++ [fields]
  where
    header = B8.toStrict $ AP.encodePretty' apConf h
    fields = join $ map B8.pack hashLineFields

hWriteHeader :: Handle -> [String] -> IO ()
hWriteHeader hdl es = do
  hdr <- makeHeaderNow es
  B8.hPutStr hdl $ renderHeader hdr

--- footer ---

data Footer = Footer
  { scanEnd  :: Integer
  -- TODO , nErrors  :: Int
  -- TODO , nScanned :: Int -- TODO integer?
  }
  deriving (Eq, Read, Show, Generic)

instance ToJSON   Footer
instance FromJSON Footer

makeFooterNow :: IO Footer
makeFooterNow = do
  scanEnd <- now
  let footer = Footer
        { scanEnd  = scanEnd
        -- , nScanned = nOK
        -- , nErrors    = nErr
        }
  return footer

renderFooter :: Footer -> B8.ByteString
renderFooter f = B8.unlines $ commentLines $ B8.lines footer
  where
    footer = B8.toStrict $ AP.encodePretty' apConf f

hWriteFooter :: Handle -> IO ()
hWriteFooter hdl = do
  ftr <- makeFooterNow
  B8.hPutStr hdl $ renderFooter ftr
