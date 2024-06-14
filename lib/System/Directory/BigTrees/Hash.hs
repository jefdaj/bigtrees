{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE QuasiQuotes                #-}


{-|
Description: Hashes and hash digests.

Long description here.
-}

-- TODO hashHashes should be hashDir
-- TODO should it also hash filenames?
-- TODO convert everything here to UTF-8?

module System.Directory.BigTrees.Hash

  ( Hash(..)
  , digestLength
  , prettyHash
  , hashBytes
  , hashString
  , hashFile
  , looksLikeAnnexPath
  , hashFromAnnexPath
  , hashSymlinkLiteral
  , hashSymlinkTarget

  -- tests
  , unit_hash_ByteString
  , unit_hash_empty_file
  , unit_hash_file_contents
  , unit_hash_image

  )
  where

import Control.DeepSeq (NFData)
import qualified Crypto.Hash as CH
import Crypto.Hash.Algorithms (SHA256 (SHA256))
import Crypto.Hash.IO (hashMutableFinalize, hashMutableInitWith, hashMutableUpdate)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Short as BS
import Data.Functor ((<&>))
import Data.Hashable (Hashable (..))
import Data.List (isInfixOf, isPrefixOf)
import Data.List.Split (splitOn)
import GHC.Generics (Generic)
import Streaming (Of, Stream)
import qualified Streaming.ByteString.Char8 as Q
import qualified Streaming.Prelude as S
import qualified System.Directory.OsPath as SDO
import System.FilePath (takeFileName)
import System.OsPath (takeBaseName, takeDirectory, (</>), encodeFS, decodeFS)
import System.IO.Temp (emptySystemTempFile, writeSystemTempFile)
-- import System.Posix.Files (readSymbolicLink)
import Test.HUnit (Assertion, (@=?))
import Test.QuickCheck (Arbitrary (..), Gen, arbitrary, choose, resize, sized, suchThat)
import Test.QuickCheck.Instances.ByteString ()
import Text.Regex.TDFA ((=~))
import TH.Derive (Deriving, derive)
import qualified System.OsPath as OSP
import System.OsPath (OsPath)
import System.OsString.Internal.Types -- TODO specifics
import qualified System.File.OsPath as SFO


{- Checksum (sha256sum?) of a file or folder.
 - For files, should match the corresponding git-annex key.
 - TODO would storing it in a more efficient way help?
 - TODO would adding timestamps or number of files help?
 - note: regular bytestrings here cause memory fragmentation/space leaks
 -}
newtype Hash
  = Hash { unHash :: BS.ShortByteString }
  deriving (Eq, Generic, NFData, Ord, Read, Show)

-- This is unrelated to BigTrees's hashing. It's required to use Data.HashMap
instance Hashable Hash
  where
    hashWithSalt :: Int -> Hash -> Int
    hashWithSalt n h = hashWithSalt n (unHash h)

-- TODO remove?
instance Arbitrary Hash where

  arbitrary :: Gen Hash
  arbitrary = fmap hashBytes (arbitrary :: Gen B8.ByteString)

  shrink :: Hash -> [Hash]
  shrink _ = []

digestLength :: Int
digestLength = 20

-- TODO remove? looks like it might already be in the proper OsString format with unHash
-- TODO actual Pretty instance
-- TODO how many chars to display? git uses two groups of 7 like this
-- prettyHash (Hash h) = firstN h ++ "..." ++ lastN h
prettyHash :: Hash -> B.ByteString
prettyHash = BS.fromShort . unHash

compress :: B.ByteString -> BS.ShortByteString
compress = BS.toShort . B.take digestLength . B64.encode

-- TODO no need to B.copy here?
hashBytes :: B.ByteString -> Hash
hashBytes = Hash . compress . B.pack . show . (CH.hash :: B.ByteString -> CH.Digest CH.SHA256)

-- TODO would digestFromByteString be faster?
-- TODO bug! digests come out unreadable :(
hashBytesStreaming :: BL.ByteString -> IO Hash
hashBytesStreaming bs = do
  ctx <- hashMutableInitWith SHA256
  let chunked :: Stream (Of B.ByteString) IO ()
      chunked = Q.toChunks $ Q.fromLazy bs
  S.mapM_ (hashMutableUpdate ctx) chunked
  final <- hashMutableFinalize ctx
  return $ Hash $ compress $ B.pack $ show final

hashString :: String -> Hash
hashString = hashBytes . B.pack

-- Hashes the target path itself as a string, because contents are either
-- missing or outside the tree being scanned.
-- TODO wouldn't it be more correct to hash literal bytes rather than decoding?
hashSymlinkLiteral :: OsPath -> IO Hash
hashSymlinkLiteral path = do
  op <- SDO.getSymbolicLinkTarget path
  op' <- decodeFS op
  return $ hashString op'

-- Hashes target file contents.
-- TODO will it work recursively?
-- TODO guard against this pointing outside the tree being scanned;
--      we want to treat that as a broken link instead
-- TODO fails on dirs?
hashSymlinkTarget :: OsPath -> IO Hash
hashSymlinkTarget path = do
  target <- SDO.getSymbolicLinkTarget path
  let p' = takeDirectory path </> target
  hashFileContentsStreaming p'

-- TODO Was the .git/annex/objects prefix important?
--      If not, don't want to make matching the actual content files any harder by adding it
looksLikeAnnexPath :: FilePath -> Bool
looksLikeAnnexPath p = (takeFileName p) =~ regex
  where
    -- TODO check that this isn't missing any variations
    regex = "^SHA256E-[a-z0-9]{2,}--[0-9a-f]{64}(\\..*)?$" :: String

-- Tests that this looks like an annex path, then returns the implied sha256sum.
-- TODO proper fmap idiom here
-- TODO extract a match from the regex rather than separately here
hashFromAnnexPath :: OsPath -> IO (Maybe Hash)
hashFromAnnexPath p = do
  p' <- decodeFS p
  return $ if looksLikeAnnexPath p' then Just $ pHash p' else Nothing
  where
    pHash = Hash . compress . B.pack . last . splitOn "--" . head . splitOn "." . takeFileName

-- see: https://stackoverflow.com/a/30537010
-- hashFileContents :: OsPath -> IO Hash
-- hashFileContents path = do -- TODO hashFileContents
--   !sha256sum <- fmap hashBytes $ B.readFile path
--   -- when verbose (putStrLn $ sha256sum ++ " " ++ path)
--   return $ Hash sha256sum

-- based on https://gist.github.com/michaelt/6c6843e6dd8030e95d58
-- TODO show when verbose?
hashFileContentsStreaming :: OsPath -> IO Hash
hashFileContentsStreaming path = SFO.readFile path >>= hashBytesStreaming

-- Hashes if necessary, but tries to read it from a link first
-- Note that this can only print file hashes, not the whole streaming trees format
-- TODO remove the unused verbose flag?
-- TODO handle case where the file is itself a git-annex content file!
hashFile :: Bool -> OsPath -> IO Hash
hashFile _ path = hashFileContentsStreaming path

-- note: no need to explicitly match against sha256sum because the manual examples cover that
-- TODO but make sure they match manually! how to handle the base64 encoding part?

-----------
-- tests --
-----------

unit_hash_ByteString :: Assertion
unit_hash_ByteString = unHash (hashBytes "a bytestring") @=? "YTI3MDBmODFhZWE2ZjBm"

-- TODO clean up tmpfile handling here
unit_hash_empty_file :: Assertion
unit_hash_empty_file = do
  f <- emptySystemTempFile "empty"
  f' <- encodeFS f
  h <- hashFile False f'
  SDO.removePathForcibly f'
  unHash h @=? "ZTNiMGM0NDI5OGZjMWMx"

-- TODO clean up tmpfile handling here
unit_hash_file_contents :: Assertion
unit_hash_file_contents = do
  -- filename changes each time, proving only the contents are hashed
  -- (and also that the hash algo is still working properly)
  f <- writeSystemTempFile "bigtrees" "file contents should be hashed"
  f' <- encodeFS f
  h <- hashFile False f'
  SDO.removePathForcibly f'
  unHash h @=? "MTVjMzcwNmJjODQzYTg0"

-- TODO should the source code really be used this way?
unit_hash_image :: Assertion
unit_hash_image = do
  h <- hashFile False [OSP.osp|bigtrees.png|]
  unHash h @=? "NzdkN2M0OGYxZGViOTY5"

-- TODO unit_hash_dir
-- TODO unit_hash_dir_random_filenames
-- TODO unit_hash_empty_dir
