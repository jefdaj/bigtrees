{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module System.Directory.BigTrees.HashTree.Types where

import Control.DeepSeq
import Data.Store (Store (..))
import GHC.Generics (Generic)
import System.Directory.BigTrees.Hash
import System.Directory.BigTrees.Name (Name (..))
import TH.Derive


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
