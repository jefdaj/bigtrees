{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Directory.BigTrees.HeadFoot
  ( commentLineP
  , scanSeconds

  , Header (..)
  , headerP
  , parseHeader
  , readHeader
  , assertCompatibleTreeFormatHeader
  , hWriteHeader

  , Footer (..)
  , footerP
  , parseFooter
  , hWriteFooter
  )
  where

import Control.DeepSeq (NFData)
import Data.Version (showVersion)
import Paths_bigtrees (version)
import System.Environment (getEnv, getProgName)
import System.Info (arch, compilerName, fullCompilerVersion, os)
-- import System.FilePath.Glob (Pattern)
import Data.Time.Clock.POSIX (getPOSIXTime)
-- import Data.Time.Clock (secondsToDiffTime)
-- import Control.Monad (forM, replicateM, when)
import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.Aeson.Encode.Pretty as AP
import Data.Attoparsec.ByteString.Char8 (Parser, anyChar, char, endOfLine, manyTill, sepBy')
-- import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.Attoparsec.Combinator (lookAhead)
import qualified Data.ByteString.Char8 as B8
import Data.Functor ((<&>))
import Data.String.Utils (replace)
import GHC.Generics (Generic)
import System.Directory.BigTrees.HashLine.Base
import qualified System.File.OsPath as SFO
import System.IO (Handle, IOMode (..), hGetLine, hIsEOF)
import System.OsPath (OsPath)
import System.Directory.BigTrees.Logging (LogCfg(..), addLogContext, die)

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

instance NFData   Header
instance ToJSON   Header
instance FromJSON Header

-- update when changing anything that might break the parser, and keep track of
-- prev versions with git tags in order to implement upgrade fns later
-- TODO use bigtrees version when it was changed instead of old date format?
currentTreeFormat :: Int
currentTreeFormat = 251103

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
    fields = joinCols $ map B8.pack hashLineFields

hWriteHeader :: Handle -> [String] -> IO ()
hWriteHeader hdl es = do
  hdr <- makeHeaderNow es
  B8.hPutStr hdl $ renderHeader hdr -- <> B8.singleton '\NUL' TODO put back

--- footer ---

data Footer = Footer
  { scanEnd  :: Integer
  -- TODO , nErrors  :: Int
  -- TODO , nScanned :: Int -- TODO integer?
  }
  deriving (Eq, Read, Show, Generic)

instance NFData   Footer
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

----------------------------
-- moved from other files --
----------------------------

footerP = do
  footerLines <- sepBy' commentLineP endOfLine -- <* endOfLine
  case parseFooter footerLines of
    Nothing -> fail "failed to parse footer"
    Just h  -> return h

-- The main Attoparsec parser(s) can separate the commented section,
-- then the uncommented JSON is handled here.
-- TODO is it an Either?
parseFooter :: [String] -> Maybe Footer
parseFooter = decode . B8.fromStrict . B8.pack . unlines . map (replace "# " "")

--- read header info from the beginning of the file ---

readCommentLines :: Handle -> Int -> IO [String]
readCommentLines h maxLines = readLines maxLines []
  where
    readLines 0 acc = return (reverse acc)
    readLines n acc = do
      eof <- hIsEOF h
      if eof
        then return (reverse acc)
        else do
          line <- hGetLine h
          if isCommentLine line
            then readLines (n-1) (line:acc)
            else return (reverse acc)  -- Stop at first non-comment

-- TODO document 100 line limit
-- TODO SFO.withBinaryFile?
readHeader :: LogCfg -> OsPath -> IO (Maybe Header)
readHeader lCfg path =
  SFO.withBinaryFile path ReadMode $ \h -> do
    ls <- readCommentLines h 100
    return $ parseHeader lCfg ls

-- Header is the same, except we have to lob off the final header line
-- TODO also confirm it looks as expected? tree format should be enough tho
-- TODO DRY this out vs headerP below
parseHeader :: LogCfg -> [String] -> Maybe Header
parseHeader _ s = case s of
  [ ] -> Nothing -- should never happen, right?
  [_] -> Nothing -- should never happen, right?
  ls  -> decode $ B8.fromStrict $ B8.pack $ unlines $ map (replace "# " "") $ init ls

isCommentLine :: String -> Bool
isCommentLine ('#':_) = True
isCommentLine _       = False

-- commentLineP :: Parser B8.ByteString [Char]
commentLineP = do
  _ <- char '#'
  manyTill anyChar $ lookAhead endOfLine

-- TODO come up with a policy for how far back old formats will be supported
assertCompatibleTreeFormatHeader :: LogCfg -> Header -> a -> a
assertCompatibleTreeFormatHeader lCfg hdr rtn =
  let oldestOk = 251103
      lCfg' = addLogContext lCfg "assertCompatibleTreeFormat"
      showF = B8.pack $ show $ treeFormat hdr
      msg = "file is in old tree format " <> showF <> ", which will fail to parse"
  in if (treeFormat hdr < oldestOk)
    then die lCfg' msg
    else rtn

-- TODO assert compatible here too?
headerP lCfg = do
  -- TODO handle \NUL here, right?
  headerLines <- sepBy' commentLineP endOfLine <* endOfLine
  case parseHeader lCfg headerLines of
    Nothing -> fail "failed to parse header"
    Just h  -> return h
