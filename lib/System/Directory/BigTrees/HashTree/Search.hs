{-# LANGUAGE DeriveGeneric #-}

module System.Directory.BigTrees.HashTree.Search where

import Control.Monad (msum)
import Data.Maybe (isJust)
import System.Directory.BigTrees.Hash (Hash)
import System.Directory.BigTrees.Name (Name, os2ns)
import System.Directory.BigTrees.Util (pathComponents)
import System.OsPath (OsPath, joinPath)

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import System.Directory.BigTrees.HashTree.Base -- TODO specifics


-- | All the info relevant to searching a tree. Used in different ways when
-- building a tree, reading it from a .bigtree file, finding paths in it, and
-- making a dupe map.
data SearchConfig = SearchConfig
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

instance NFData SearchConfig


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
