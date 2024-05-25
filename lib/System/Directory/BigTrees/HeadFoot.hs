{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
scanSeconds (h, f) = endTime f - startTime h

treeInfo :: (Header, Footer) -> B8.ByteString
treeInfo = undefined

--- header ---

data Header = Header
  { compiler :: String -- compiler, version
  , exclude  :: [String]
  , format   :: Int
  , locale   :: String
  , program  :: String -- format version string, from cabal file
  , startTime    :: Integer
  , system   :: String -- os, arch TODO uname?
  }
  deriving (Eq, Read, Show, Generic)

instance ToJSON   Header
instance FromJSON Header

makeHeaderNow :: [String] -> IO Header
makeHeaderNow es = do
  progName  <- getProgName
  startTime <- now
  lang      <- getEnv "LANG" -- TODO locale? LC_ALL? others?
  let header = Header
        { compiler  = unwords [compilerName, showVersion fullCompilerVersion]
        , exclude   = es
        , format    = 2 -- update when changing anything that breaks parser
        , locale    = lang
        , program   = unwords [progName, showVersion version]
        , startTime = startTime
        , system    = unwords [os, arch]
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
  B8.hPutStrLn hdl $ renderHeader hdr

--- footer ---

data Footer = Footer
  { endTime  :: Integer
  -- TODO , nErrors  :: Int
  -- TODO , nScanned :: Int -- TODO integer?
  }
  deriving (Eq, Read, Show, Generic)

instance ToJSON   Footer
instance FromJSON Footer

makeFooterNow :: IO Footer
makeFooterNow = do
  endTime <- now
  let footer = Footer
        { endTime  = endTime
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
  B8.hPutStrLn hdl $ renderFooter ftr
