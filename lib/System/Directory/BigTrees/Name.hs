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
  , breadcrumbs2bs

  -- tests
  -- TODO document tests as a group
  -- , myShrinkText
  -- , isValidName TODO rewrite for OsPath
  , OSP.isValid
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
import qualified System.FilePath as SF
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
import qualified System.OsPath as OSP
import Test.QuickCheck.Instances.ByteString
import qualified Data.ByteString.Short as SBS
import qualified System.OsPath.Internal as OSPI
import qualified Data.ByteString.Char8 as B8

-- | An element in a FilePath. My `Name` type is defined as `OsPath` for
-- efficiency, but what it really means is "OsPath without slashes". Based on
-- the one in `System.Directory.Tree`. The newtype is needed to prevent
-- overlapping with the standard Arbitrary Text instance in the tests. There's
-- no point using OsPath here because Windows is already unsupported.
-- TODO why doesn't the tree link work right
newtype Name
  = Name { unName :: SBS.ShortByteString }
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

instance Arbitrary Name where
  arbitrary = Name <$> ((arbitrary :: Gen SBS.ShortByteString) `suchThat` isValidName)

  -- TODO shrink weird chars to ascii when possible, so we can tell it's not an encoding error
  -- TODO should this use https://hackage.haskell.org/package/quickcheck-unicode-1.0.1.0/docs/Test-QuickCheck-Unicode.html
  shrink :: Name -> [Name]
  shrink (Name t) = Name <$> filter isValidName (shrink t)

-- TODO is there ever another separator, except on windows?
-- TODO use this in the arbitrary filepath instance too?
-- TODO is GHC rejecting some of these??
isValidName :: SBS.ShortByteString -> Bool
isValidName b
  =  notElem b ["", ".", ".."]
  && (not $ "/" `SBS.isInfixOf` b) -- the byte should be \x2f or 47
  && (not $ "\NUL" `SBS.isInfixOf` b)

-- * Convert paths to/from names
--
-- $convertnamespaths
--
-- Functions for converting between `Name`s and (regular Haskell) `FilePath`s.
-- They should work on Linux and MacOS.

-- | Convert a `Name` to a `FilePath`
-- n2fp :: Name -> OSP.OsPath -- TODO shit, do i need to use OsPath in directory-tree too?
-- n2fp = unName

-- TODO this should actually convert to a list of names, right?
-- TODO and does that make it more like components?
-- | Convert a `FilePath` to a `Name`
fp2n :: FilePath -> Name
fp2n = Name . SBS.toShort . B8.pack

-- TODO better name
-- TODO does this really need IO? I thought it was just MonadFail
n2op :: Name -> IO OSP.OsPath
n2op = OSPI.fromBytes . SBS.fromShort . unName

n2bs :: Name -> B8.ByteString
n2bs = SBS.fromShort . unName

-- TODO double-check that this will be the right byte for posix paths (47)
joinNames :: [Name] -> B8.ByteString
joinNames = B8.intercalate (B8.singleton '/') . map n2bs

-- Breadcrumbs are a list of names leading to the current node, like an anchor
-- path but sorted in reverse order because we want `cons` to be fast.
-- TODO custom type for this?
-- TODO does this really need IO?
breadcrumbs2bs :: [Name] -> B8.ByteString
breadcrumbs2bs = joinNames . reverse

-- | I plan to make a PR to the directory-tree package adding TreeName
-- TODO should probably unify FilePath and Name again and make this non-orphan
-- instance DT.IsName Name where
--   n2p = n2fp
--   p2n = fp2n

-- n2bs :: Name -> BU.ByteString
-- n2bs = BU.fromString . n2fp

-- TODO should this have the option for a decoding error?
-- bs2n :: BU.ByteString -> Name
-- bs2n = fp2n . BU.toString

-- TODO this is impossible, right? get rid of it
-- prop_roundtrip_Name_to_String :: Name -> Bool
-- prop_roundtrip_Name_to_String n = read (show n) == n

-- prop_roundtrip_Name_to_filepath :: Name -> Bool
-- prop_roundtrip_Name_to_filepath n = fp2n (n2fp n) == n

-- fails if there's an error writing the file,
-- or if after writing it doesn't exist
roundtripNameToFileName :: Name -> IO ()
roundtripNameToFileName n =
  withSystemTempDirectory "bigtrees" $ \d -> do
    d' <- OSP.encodeFS d
    n' <- n2op n -- TODO why is fromShort needed? seems redundant
    let f = d' OSP.</> n'
    SFO.writeFile f "this is a test"
    _ <- SFO.readFile f
    return ()

prop_roundtrip_Name_to_FileName :: Property
prop_roundtrip_Name_to_FileName = monadicIO $ do
  n <- pick arbitrary
  run $ roundtripNameToFileName n
  assert True
