{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

{-|
Description: Custom FileName type

My `FileName` type is defined as `Text` for efficiency, but what it really
means is "Text without slashes or null chars". So I have to define my own
Arbitrary instance here.
-}

-- TODO why is the not . null thing required to prevent empty strings? list1 should be enough
-- TODO wait, is the empty string also a valid filename?

module System.Directory.BigTrees.FileName where

import Control.DeepSeq
import Data.Store (Store (..))
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as OS
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import TH.Derive

-- from System.Directory.Tree --

-- | an element in a FilePath:
-- The newtype is needed to prevent overlapping with the standard Arbitrary
-- Text instance in the tests
newtype FileName
  = FileName T.Text
  deriving (Eq, Generic, Ord, Read, Show)

deriving instance NFData FileName

-- https://hackage.haskell.org/package/store-0.7.2/docs/Data-Store-TH.html
$($(derive [d|
  instance Deriving (Store FileName)
  |]))

instance Arbitrary FileName where
  arbitrary = FileName <$> (arbitrary :: Gen T.Text) `suchThat` validFileName
  shrink (FileName t) = FileName <$> filter validFileName (shrink t)

validFileName :: T.Text -> Bool
validFileName t = notElem t ["", ".", ".."]
               && (not . T.any (== '/')) t -- no separators
               && (OS.valid . OS.fromText) t
