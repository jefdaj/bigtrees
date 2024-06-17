{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-|
Description: Name handling

I've had issues properly encoding some filenames using the standard libraries.
This fixes most of them.
This module is for the custom `Name` type used in trees.
There is also a `Path` module, but it holds unrelated-in-principle utility functions.

-}

-- TODO actually remove the overview at the top? the individual types + functions make more sense

-- TODO why is the not . null thing required to prevent empty strings? list1 should be enough
-- TODO wait, is the empty string also a valid filename?

module System.Directory.BigTrees.Name

  -- TODO document these individually
  ( Name(..)
  , NamesFwd
  , NamesRev

  , n2sbs
  , sbs2n
  , sbs2op
  , fp2n
  , fp2ns
  , n2bs
  , bs2n
  , breadcrumbs2bs
  , joinNames
  , names2bs
  , os2ns
  , op2ns
  , op2bs
  , bs2op

  , nameP

  -- tests
  -- TODO document tests as a group
  , isValidName
  , roundtripNameToFileName
  , roundtripNameToDirName
  , prop_roundtrip_Name_to_file_name
  , prop_roundtrip_Name_to_dir_name

  )
  where

import Control.DeepSeq (NFData)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Short as BS
import Data.List (isInfixOf, isPrefixOf, nub)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Filesystem.Path.CurrentOS as OS
import GHC.Generics (Generic)
import Prelude hiding (log)
import qualified System.Directory.Tree as DT
-- import qualified System.FilePath as SF
import System.Info (os)
import System.IO.Temp (withSystemTempDirectory)
import System.Path.NameManip (absolute_path, guess_dotdot)
import System.Posix.Files (getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink)
import Test.QuickCheck (Arbitrary (..), Gen, Property, suchThat)
import Test.QuickCheck.Arbitrary ()
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)
import TH.Derive (Deriving, derive)

-- attempt at proper new string types:
-- import System.FilePath ((</>))
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as SBS
import qualified System.Directory.OsPath as SDO
import qualified System.File.OsPath as SFO
import qualified System.OsPath as SOP
import qualified System.OsPath.Internal as SOPI
import qualified System.OsString as SOS
import qualified System.OsString.Internal.Types as SOS
import Test.QuickCheck.Instances.ByteString

import Data.Attoparsec.ByteString (skipWhile)
import Data.Attoparsec.ByteString.Char8 (Parser, anyChar, char, choice, digit, endOfInput,
                                         endOfLine, isEndOfLine, manyTill, parseOnly, take,
                                         takeTill)
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.Attoparsec.Combinator (lookAhead, option, sepBy')
import System.OsPath (OsPath)

-- | An element in a FilePath. My `Name` type is defined as `OsPath` for
-- efficiency, but what it really means is "OsPath without slashes". Based on
-- the one in `System.Directory.Tree`. The newtype is needed to prevent
-- overlapping with the standard Arbitrary Text instance in the tests. There's
-- no point using OsPath here because Windows is already unsupported.
-- TODO why doesn't the tree link work right
newtype Name
  = Name { unName :: SOS.OsString } -- TODO OsPath? It's an alias without an exposed constructor
  deriving (Eq, Generic, Ord, Show)

deriving instance NFData Name

-- TODO does the standard instance already shrink each char?
-- TODO does the 2nd guard for going to single Chars help?
--
-- >>> filter isValidName $ myShrinkText "\US"
-- ["abcABC123 \n"]
--
-- TODO rewrite for OsPath
-- myShrinkText :: T.Text -> [T.Text]
-- myShrinkText t
--   | T.length t == 1 = map T.pack $ (\[c] -> [shrink c]) $ T.unpack t
--   | T.length t < 4 = map (\c -> T.pack [c]) $ nub $ T.unpack t
--   | otherwise = shrink t

-- TODO shrink weird chars to ascii when possible, so we can tell it's not an encoding error
-- TODO try https://hackage.haskell.org/package/quickcheck-unicode-1.0.1.0/docs/Test-QuickCheck-Unicode.html
instance Arbitrary Name where
  arbitrary = Name <$> (oss `suchThat` isValidName)
    where
      sbs = arbitrary :: Gen SBS.ShortByteString
      oss = (SOS.OsString . SOS.PosixString) <$> sbs

  shrink :: Name -> [Name]
  shrink = (map Name . filter isValidName) <$> (oss . n2sbs)
    where
      sbs = shrink :: SBS.ShortByteString -> [SBS.ShortByteString]
      oss = map (SOS.OsString . SOS.PosixString) <$> sbs

-- TODO use this in the arbitrary filepath instance too?
-- Checking for '/' explicitly turns out to be necessary because
-- SOP.splitDirectories will still return a length-1 list if there's a slash at
-- the end of the name. Then writeFile et al will throw "inappropriate type".
isValidName :: SOS.OsString -> Bool
isValidName s
  = SOP.isValid s
  && not ("/" `SBS.isInfixOf` (SOS.getPosixString $ SOS.getOsString s))
  && notElem s [[SOS.osstr|.|], [SOS.osstr|..|]]

-- * Convert paths to/from names
--
-- $convertnamespaths
--
-- Functions for converting between `Name`s and (regular Haskell) `FilePath`s.
-- They should work on Linux and MacOS.

n2sbs :: Name -> SBS.ShortByteString
n2sbs = SOS.getPosixString . SOS.getOsString . unName

-- | Note this does NOT check whether it's a valid Name.
sbs2n :: SBS.ShortByteString -> Name
sbs2n = Name . sbs2op

sbs2op :: SBS.ShortByteString -> OsPath
sbs2op = SOS.OsString . SOS.PosixString

-- | Convert a `FilePath` to a `Name` using the current filesystem's encoding,
-- or explain why the conversion failed.
fp2n :: FilePath -> IO (Either String Name)
fp2n fp = do
  ns <- fp2ns fp -- TODO catch error here and wrap it in Left too
  return $ case ns of
    []       -> Left "fp2n with null path"
    [Name n] -> if isValidName n then Right (Name n) else Left $ "invalid name: " ++ show n
    ns       -> Left "fp2n with slash in path"

-- | Convert a `FilePath` to a list of `Name`s using the current filesystem's encoding.
-- TODO or explain why the conversion failed?
fp2ns :: FilePath -> IO [Name]
fp2ns fp = do
  osstr <- SOS.encodeFS fp -- TODO catch error here and wrap in Left?
  let osstrs = SOP.splitDirectories osstr
  return $ map Name osstrs

-- | Direct conversion from a Name to a ByteString for serializing.
-- TODO make it an instance of Bytable, Binary, similar?
n2bs :: Name -> B8.ByteString
n2bs = SBS.fromShort . n2sbs

-- | Direct conversion from a ByteString to a Name for deserializing.
-- TODO make it an instance of Bytable, Binary, similar?
bs2n :: B8.ByteString -> Name
bs2n = sbs2n . SBS.toShort

os2ns :: SOS.OsString -> [Name]
os2ns = map Name . SOP.splitDirectories

op2ns :: SOP.OsPath -> [Name]
op2ns = os2ns

-- | Extra type alias to distinguish lists of Names representing a path in
-- forward vs reverse order. Both can be converted to/from OsPaths.
type NamesFwd = [Name]

-- | NamesRev are a list of names leading to the current node, like an anchor
-- path but sorted in reverse order because we want `cons` to be fast.
-- Sometimes called "breadcrumbs", although I'll try to be more consistent.
type NamesRev = [Name]

-- TODO rename?
breadcrumbs2bs :: NamesRev -> B8.ByteString
breadcrumbs2bs = joinNames . reverse

-- TODO was this needed for anything else?
-- TODO can it be done better via SOP.joinPath?
joinNames :: [Name] -> B8.ByteString
joinNames = B8.intercalate (B8.singleton '/') . map n2bs

names2bs :: NamesFwd -> B8.ByteString
names2bs = SBS.fromShort . SOS.getPosixString . SOS.getOsString . SOP.joinPath . map unName

-- TODO is this valid?
op2bs :: OsPath -> B8.ByteString
op2bs = SBS.fromShort . SOS.getPosixString . SOS.getOsString

-- TODO is this valid?
bs2op :: B8.ByteString -> OsPath
bs2op = SOS.OsString . SOS.PosixString . SBS.toShort

-- TODO is Attoparsec.ByteString suitable for this, or do I need to parse them some other way?
nameP :: Parser Name
nameP = do
  -- TODO sepP here?
  bs <- takeTill (== '\NUL')
  _  <- char '\NUL'
  _  <- option undefined $ char '\t' -- TODO if this works, move sepP from HashLine
  return $ bs2n bs

-- Fails if there's an error writing the file, or if after writing it doesn't
-- exist. Example manual usage:
--
-- >>> ns <- generate (resize 99 $ arbitrary :: Gen [Name])
-- >>> fmap (all id) $ mapM_ (roundtripNameToFileName False) ns
-- >>> True
--
-- Set verbose=True to show the paths, but beware! They might mess up your terminal.
--
-- TODO is there a standard variant of `all` that works like this?
--
roundtripNameToFileName :: Bool -> Name -> IO Bool
roundtripNameToFileName verbose n =
  withSystemTempDirectory "bigtrees" $ \d -> do
    d' <- SOP.encodeFS d
    let f = d' SOP.</> unName n
    let txt = "this is a test"
    SFO.writeFile f txt
    when verbose $ print f
    txt' <- SFO.readFile f
    return $ txt == txt'

prop_roundtrip_Name_to_file_name :: Property
prop_roundtrip_Name_to_file_name = monadicIO $ do
  n <- pick arbitrary
  ok <- run $ roundtripNameToFileName False n
  assert ok

roundtripNameToDirName :: Bool -> Name -> IO Bool
roundtripNameToDirName verbose n =
  withSystemTempDirectory "bigtrees" $ \tmpDir -> do
    tmpDir' <- SOP.encodeFS tmpDir
    let testDir = tmpDir' SOP.</> unName n
    SDO.createDirectory testDir
    when verbose $ print testDir
    cs <- SDO.getDirectoryContents tmpDir'
    return $ (unName n) `elem` cs

prop_roundtrip_Name_to_dir_name :: Property
prop_roundtrip_Name_to_dir_name = monadicIO $ do
  n <- pick arbitrary
  ok <- run $ roundtripNameToDirName False n
  assert ok
