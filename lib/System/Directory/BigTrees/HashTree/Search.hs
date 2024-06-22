{-# LANGUAGE DeriveGeneric #-}

module System.Directory.BigTrees.HashTree.Search where

import Control.Monad (msum)
import Data.Aeson -- (eitherDecodeFileStrict)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (isJust)
import System.Directory.BigTrees.Hash (Hash)
import System.Directory.BigTrees.HashLine (Depth (..), ModTime (..), NBytes (..), NNodes (..),
                                           TreeType (..))
import System.Directory.BigTrees.Name (Name, os2ns)
import System.Directory.BigTrees.Util (pathComponents)
import System.OsPath (OsPath, joinPath)

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import System.Directory.BigTrees.HashTree.Base

-- | All the info relevant to searching a tree. Used in different ways when
-- building a tree, reading it from a .bigtree file, finding paths in it, and
-- making a dupe map.
data SearchConfig = SearchConfig
  { minBytes       :: Maybe NBytes
  , maxBytes       :: Maybe NBytes
  , maxDepth       :: Maybe Depth
  , minDepth       :: Maybe Depth
  , minFiles       :: Maybe NNodes
  , maxFiles       :: Maybe NNodes
  , minModtime     :: Maybe ModTime
  , maxModtime     :: Maybe ModTime
  , treeTypes      :: Maybe [TreeType]
  , excludeRegexes :: [String]
  , searchRegexes  :: LabeledSearchStrings
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
  { excludeRegexes = ["\\.sw.*", "\\.DS_Store$", "\\.plist$", "\\.snakemake.*"]
  }

---------------------------
-- labeled searches file --
---------------------------

type SearchLabel = String -- TODO bytestring? newtype?

-- | We store them as Strings to keep the config simple, then compile in listTreePaths.
type SearchString = String

type LabeledSearchStrings = [(SearchLabel, [SearchString])]

-- instance ToJSON LabeledSearchStrings where toEncoding = genericToEncoding defaultOptions
-- instance FromJSON LabeledSearchStrings where parseJSON = genericParseJSON defaultOptions

parseLabeledSearchStrings :: FilePath -> IO (Either String LabeledSearchStrings)
parseLabeledSearchStrings = eitherDecodeFileStrict

data Search2 = Search2
  { dirContainsPath       :: Maybe String
  , baseNameMatchesRegex  :: Maybe String
  , wholeNameMatchesRegex :: Maybe String
  }
  deriving (Read, Show, Generic)

instance ToJSON Search2 where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

instance FromJSON Search2 where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

type LabeledSearch2 = [(SearchString, [Search2])]

parseLabeledSearch2 :: FilePath -> IO (Either String LabeledSearch2)
parseLabeledSearch2 = eitherDecodeFileStrict

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
