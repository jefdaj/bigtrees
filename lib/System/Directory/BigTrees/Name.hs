{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE QuasiQuotes                #-}
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
  , fp2n
  , n2bs
  , n2op
  , op2n
  , op2ns
  , breadcrumbs2bs

  -- tests
  -- TODO document tests as a group
  -- , myShrinkText
  -- , isValidName TODO rewrite for OsPath
  , SOP.isValid
  , roundtripNameToFileName
  -- , prop_roundtrip_Name_to_String
  , prop_roundtrip_Name_to_FileName
  -- , prop_roundtrip_Name_to_filepath

  )
  where

import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Short as BS
import Data.List (isInfixOf, isPrefixOf, nub)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Filesystem.Path.CurrentOS as OS
import GHC.Generics (Generic)
import Prelude hiding (log)
import System.Directory (canonicalizePath, getHomeDirectory)
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
import qualified System.File.OsPath as SFO
import qualified System.OsPath as SOP
import qualified System.OsString as SOS
import qualified System.OsString.Internal.Types as SOS
import Test.QuickCheck.Instances.ByteString
import qualified Data.ByteString.Short as SBS
import qualified System.OsPath.Internal as SOPI
import qualified Data.ByteString.Char8 as B8

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
      oss = (SOS.OsString . SOS.PosixString) <$> sbs
      sbs = arbitrary :: Gen SBS.ShortByteString

  shrink :: Name -> [Name]
  shrink (Name t) = Name <$> filter isValidName (shrink t)

-- TODO use this in the arbitrary filepath instance too?
isValidName :: SOS.OsString -> Bool
isValidName s
  = SOP.isValid s
  && length (SOP.splitDirectories s) == 1
  && notElem s [[SOS.osstr|.|], [SOS.osstr|..|]]

-- * Convert paths to/from names
--
-- $convertnamespaths
--
-- Functions for converting between `Name`s and (regular Haskell) `FilePath`s.
-- They should work on Linux and MacOS.

-- | Convert a `FilePath` to a `Name` using the current filesystem's encoding,
-- or explain why the conversion failed.
fp2n :: FilePath -> IO (Either String Name)
fp2n fp = do
  ns <- fp2ns fp -- TODO catch error here and wrap it in Left too
  case ns of
    []  -> Left "fp2n with null path"
    [n] -> if isValidName n then Right n else Left $ "invalid name: " ++ show n
    ns  -> Left "fp2n with slash in path"

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
n2bs = SBS.fromShort . SOS.getOsString . unName

-- | Direct conversion from a ByteString to a Name for deserializing.
-- TODO make it an instance of Bytable, Binary, similar?
bs2n :: B8.ByteString -> Name
bs2n = Name . SOS.OsString . SBS.toShort

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
names2bs = SBS.fromShort . SOS.getOsString . SOP.joinPath

-- fails if there's an error writing the file,
-- or if after writing it doesn't exist
roundtripNameToFileName :: Name -> IO ()
roundtripNameToFileName n =
  withSystemTempDirectory "bigtrees" $ \d -> do
    d' <- SOP.encodeFS d
    n' <- n2op n -- TODO why is fromShort needed? seems redundant
    let f = d' SOP.</> n'
    SFO.writeFile f "this is a test"
    _ <- SFO.readFile f
    return ()

prop_roundtrip_Name_to_FileName :: Property
prop_roundtrip_Name_to_FileName = monadicIO $ do
  n <- pick arbitrary
  run $ roundtripNameToFileName n
  assert True
