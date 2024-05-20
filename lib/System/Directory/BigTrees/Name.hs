{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
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
  , n2fp

  -- tests
  -- TODO document tests as a group
  , prop_roundtrip_name_to_filename
  , prop_roundtrip_name_to_filepath

  )
  where

import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import Data.List (isInfixOf, isPrefixOf)
import Data.Store (Store (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Filesystem.Path.CurrentOS as OS
import GHC.Generics (Generic)
import Prelude hiding (log)
import System.Directory (canonicalizePath, getHomeDirectory)
import qualified System.Directory.Tree as DT
import qualified System.FilePath as SF
import System.FilePath ((</>))
import System.Info (os)
import System.IO.Temp (withSystemTempDirectory)
import System.Path.NameManip (absolute_path, guess_dotdot)
import System.Posix.Files (getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink)
import Test.HUnit (Assertion, (@=?))
import Test.QuickCheck (Arbitrary (..), Gen, Property, listOf, oneof, suchThat)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)
import TH.Derive (Deriving, derive)

-- | An element in a FilePath.
-- My `Name` type is defined as `Text` for efficiency, but what it really
-- means is "Text without slashes or null chars".
-- Based on the one in `System.Directory.Tree`.
-- The newtype is needed to prevent overlapping with the standard Arbitrary
-- Text instance in the tests.
-- TODO why doesn't the tree link work right
newtype Name
  = Name T.Text
  deriving (Eq, Generic, Ord, Read, Show)

deriving instance NFData Name

-- https://hackage.haskell.org/package/store-0.7.2/docs/Data-Store-TH.html
$($(derive [d|
  instance Deriving (Store Name)
  |]))

instance Arbitrary Name where
  arbitrary :: Gen Name
  arbitrary = Name <$> (arbitrary :: Gen T.Text) `suchThat` validName
  shrink :: Name -> [Name]
  shrink (Name t) = Name <$> filter validName (shrink t)

-- TODO is there ever another separator, except on windows?
-- TODO use this in the arbitrary filepath instance too?
validName :: T.Text -> Bool
validName t = notElem t ["", ".", ".."]
               && (not . T.any (== '/')) t -- no separators
               && (OS.valid . OS.fromText) t

-- * Convert paths to/from names
--
-- $convertnamespaths
--
-- Functions for converting between `Name`s and (regular Haskell) `FilePath`s.
-- They should work on Linux and MacOS.

-- | Convert a `Name` to a `FilePath`
n2fp :: Name -> FilePath
n2fp (Name t) = (if os == "darwin"
                      then B.unpack . TE.encodeUtf8
                      else T.unpack) t

-- TODO this should actually convert to a list of names, right?
-- TODO and does that make it more like components?
-- | Convert a `FilePath` to a `Name`
fp2n :: FilePath -> Name
fp2n = Name . (if os == "darwin"
                    then TE.decodeUtf8 . B.pack
                    else T.pack)

-- | I plan to make a PR to the directory-tree package adding TreeName
-- TODO should probably unify FilePath and Name again and make this non-orphan
instance DT.IsName Name where
  n2p = n2fp
  p2n = fp2n

-- n2bs :: Name -> BU.ByteString
-- n2bs = BU.fromString . n2fp

-- TODO should this have the option for a decoding error?
-- bs2n :: BU.ByteString -> Name
-- bs2n = fp2n . BU.toString

prop_roundtrip_name_to_filepath :: Name -> Bool
prop_roundtrip_name_to_filepath n = fp2n (n2fp n) == n

roundtrip_name_to_filename :: Name -> IO ()
roundtrip_name_to_filename n = withSystemTempDirectory "roundtriptemp" $ \d -> do
  let f = d </> n2fp n
  B.writeFile f "this is a test"
  _ <- B.readFile f
  return ()

prop_roundtrip_name_to_filename :: Property
prop_roundtrip_name_to_filename = monadicIO $ do
  n <- pick arbitrary
  run $ roundtrip_name_to_filename n
  assert True
