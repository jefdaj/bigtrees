{-# LANGUAGE DeriveGeneric #-}

module System.Directory.BigTrees.HashTree.Prune where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- | All the info relevant to pruning a tree from the top down, as when reading
-- (backwards) from a .bigtree file or when manipulating a `HashTree` in ghci.
data TopDownPruneConfig = TopDownPruneConfig
  { minBytes       :: Maybe Int -- ^ If <, skip. If <=, stop recursing.
  , maxBytes       :: Maybe Int -- ^ If >, skip. If >=, keep recursing.
  , maxDepth       :: Maybe Int -- ^ If <=, keep. If =, stop recursing.
  , minDepth       :: Maybe Int -- ^ If <, skip. Always keep recursing.
  , minFiles       :: Maybe Int -- ^ If <, skip. If <=, stop recursing.
  , maxFiles       :: Maybe Int -- ^ If >, skip. If <=, stop recursing.
  , minModtime     :: Maybe Int -- ^ If <, skip and stop recursing.
  , maxModtime     :: Maybe Int -- ^ If >, skip but keep recursing.
  , excludeRegexes :: [String]  -- ^ If any match, skip and stop recursing.
  , searchRegexes  :: [String]  -- ^ If any match, keep but stop recursing.
  }
  deriving (Read, Show, Eq, Ord, Generic)

instance NFData TopDownPruneConfig
