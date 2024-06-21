{-# LANGUAGE DeriveGeneric #-}

module System.Directory.BigTrees.HashTree.Search where

import Control.Monad (msum)
import Data.Maybe (isJust)
import System.Directory.BigTrees.Hash (Hash)
import System.Directory.BigTrees.Name (Name, os2ns)
import System.Directory.BigTrees.Util (pathComponents)
import System.Directory.BigTrees.HashLine (NBytes(..), NNodes(..), Depth(..), ModTime(..), TreeType(..))
import System.OsPath (OsPath, joinPath)

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import System.Directory.BigTrees.HashTree.Base -- TODO specifics


-- | All the info relevant to searching a tree. Used in different ways when
-- building a tree, reading it from a .bigtree file, finding paths in it, and
-- making a dupe map.
data SearchConfig = SearchConfig
  { minBytes       :: Maybe NBytes     -- ^ If <, skip. If <=, stop recursing.
  , maxBytes       :: Maybe NBytes     -- ^ If >, skip. If >=, keep recursing.
  , maxDepth       :: Maybe Depth      -- ^ If <=, keep. If =, stop recursing.
  , minDepth       :: Maybe Depth      -- ^ If <, skip. Always keep recursing.
  , minFiles       :: Maybe NNodes     -- ^ If <, skip. If <=, stop recursing.
  , maxFiles       :: Maybe NNodes     -- ^ If >, skip. If <=, stop recursing.
  , minModtime     :: Maybe ModTime    -- ^ If <, skip and stop recursing.
  , maxModtime     :: Maybe ModTime    -- ^ If >, skip but keep recursing.
  , treeTypes      :: Maybe [TreeType] -- ^ If any, limit to those (+ D when recursing).
  , excludeRegexes :: [String]         -- ^ If any match, skip and stop recursing.
  , searchRegexes  :: [String]         -- ^ If any match, keep but stop recursing.
  }
  deriving (Read, Show, Eq, Ord, Generic)

instance NFData SearchConfig

emptySearchConfig :: SearchConfig
emptySearchConfig = SearchConfig
  { minBytes       = Nothing
  , maxBytes       = Nothing
  , maxDepth       = Nothing
  , minDepth       = Nothing
  , minFiles       = Nothing
  , maxFiles       = Nothing
  , minModtime     = Nothing
  , maxModtime     = Nothing
  , treeTypes      = Nothing
  , excludeRegexes = []
  , searchRegexes  = []
  }

-- TODO instance Default?
defaultSearchConfig :: SearchConfig
defaultSearchConfig = emptySearchConfig
  { excludeRegexes = ["\\.sw.*", "^\\.DS_Store$", "\\.plist$", "^\\.snakemake.*"]
  }

-------------------
-- search a tree --
-------------------

treeContainsPath :: HashTree a -> OsPath -> Bool
treeContainsPath tree path = isJust $ dropTo tree $ os2ns path

dropTo :: HashTree a -> [Name] -> Maybe (HashTree a)
dropTo t [] = Just t -- TODO is that right?
dropTo t@(Err {errName=n}) (n2:_) = if n == n2 then Just t else Nothing
dropTo t@(File {nodeData=nd1}) (n2:_) = if name nd1 == n2 then Just t else Nothing
dropTo t@(Dir  {nodeData=nd1, dirContents=cs}) (n:ns)
  | name nd1 /= n = Nothing
  | otherwise = msum $ map (`dropTo` ns) cs

treeContainsHash :: HashTree a -> Hash -> Bool
treeContainsHash (Err {}) _ = False
treeContainsHash (Link {nodeData=nd1}) h2 = hash nd1 == h2
treeContainsHash (File {nodeData=nd1}) h2 = hash nd1 == h2
treeContainsHash (Dir  {nodeData=nd1, dirContents=cs}) h2
  | hash nd1 == h2 = True
  | otherwise = any (`treeContainsHash` h2) cs
