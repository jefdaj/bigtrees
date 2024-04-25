{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_HADDOCK prune #-}

{-|
Description: Custom Name type

Long description here.
-}

-- TODO why is the not . null thing required to prevent empty strings? list1 should be enough
-- TODO wait, is the empty string also a valid filename?

module System.Directory.BigTrees.Name where

import Control.DeepSeq (NFData)
import Data.Store (Store (..))
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as OS
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..), Gen, suchThat)
import Test.QuickCheck.Instances ()
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
  arbitrary = Name <$> (arbitrary :: Gen T.Text) `suchThat` validName
  shrink (Name t) = Name <$> filter validName (shrink t)

-- TODO is there ever another separator, except on windows?
-- TODO use this in the arbitrary filepath instance too?
validName :: T.Text -> Bool
validName t = notElem t ["", ".", ".."]
               && (not . T.any (== '/')) t -- no separators
               && (OS.valid . OS.fromText) t
