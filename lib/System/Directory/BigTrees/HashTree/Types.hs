{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module System.Directory.BigTrees.HashTree.Types where

import Control.DeepSeq (NFData)
import qualified Data.ByteString.Char8 as B8
import Data.Store (Store (..))
import GHC.Generics (Generic)
import System.Directory.BigTrees.Hash (Hash, hashBytes)
import System.Directory.BigTrees.HashLine (HashLine (..), IndentLevel (..), TreeType (..))
import System.Directory.BigTrees.Name (Name (..))
import Test.QuickCheck (Arbitrary (..), Gen, choose, suchThat)
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Monadic ()
import TH.Derive (Deriving, derive)


{- A tree of file names matching (a subdirectory of) the annex,
 - where each dir and file node contains a hash of its contents.
 - TODO read and write files
 - TODO would also storing the number of files in each dir help, or timestamps?
 -}
-- data HashTree = DT.AnchoredDirTree Hash
--   deriving (Eq, Read, Show)
--   TODO rename name -> path?
data HashTree a
  = File
      { name     :: !Name
      , hash     :: !Hash
      , fileData :: !a
      }
  | Dir
      { name     :: !Name
      , hash     :: Hash
      , contents :: [HashTree a]
      , nFiles   :: Int
      }
  deriving (Generic, Ord, Read, Show)

-- We only need the file decoration for testing, so we can leave it off the production types
type ProdTree = HashTree ()

-- TODO disable this while testing to ensure deep equality?
instance Eq (HashTree a) where
  t1 == t2 = hash t1 == hash t2

-- TODO once there's also a dirData, should this be BiFunctor instead?
-- TODO should this also re-hash the file, or is that not part of the fileData idea?
instance Functor HashTree where
  fmap fn f@(File {}) = f { fileData = fn (fileData f) }
  fmap fn d@(Dir  {}) = d { contents = map (fmap fn) (contents d) }

-- TODO test functor identity law

instance NFData a => NFData (HashTree a)

-- https://hackage.haskell.org/package/store-0.7.2/docs/Data-Store-TH.html
$($(derive [d|
    instance Store a => Deriving (Store (HashTree a))
    |]))

instance Arbitrary TreeType where

  arbitrary = do
    n <- choose (0,1 :: Int)
    return $ [F, D] !! n

  -- you could shrink D -> F, but not without changing the rest of the hashline
  shrink _ = []

instance Arbitrary IndentLevel where
  arbitrary = IndentLevel <$> ((arbitrary :: Gen Int) `suchThat` (>= 0))
  shrink _ = []

instance Arbitrary Hash where
  arbitrary = fmap hashBytes (arbitrary :: Gen B8.ByteString)
  shrink _ = []

-- TODO can you really have an arbitrary hashline without the rest of a tree?
instance Arbitrary HashLine where

  arbitrary = do
    tt <- arbitrary :: Gen TreeType
    il <- arbitrary :: Gen IndentLevel
    h  <- arbitrary :: Gen Hash
    n  <- arbitrary :: Gen Name
    return $ HashLine (tt, il, h, n)

  -- only shrinks the filename
  shrink (HashLine (tt, il, h, n)) = map (\n' -> HashLine (tt, il, h, n')) (shrink n)


