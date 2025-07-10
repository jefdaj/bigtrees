{-# LANGUAGE DeriveGeneric #-}

module System.Directory.BigTrees.HashTree.Search where

import Control.Monad (msum)
import Data.Aeson
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

import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString

import qualified Data.ByteString.Char8 as B8

import System.Directory.BigTrees.Name (Name (..), breadcrumbs2bs, fp2ns, n2bs)

-- import Debug.Trace

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
  , hashExcludeRegexes :: [String] -- TODO separate HashConfig from SearchConfig?
  , excludeSetPaths :: [FilePath]
  , referenceSetPaths :: [FilePath]
  , searches       :: LabeledSearches
  }
  deriving (Read, Show, Generic)

-- instance NFData SearchConfig

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
  , hashExcludeRegexes = []
  , excludeSetPaths = []
  , referenceSetPaths = []
  , searches  = []
  }

-- TODO instance Default?
defaultSearchConfig :: SearchConfig
defaultSearchConfig = emptySearchConfig
  { hashExcludeRegexes = ["\\.sw.*", "\\.DS_Store$", "\\.plist$", "\\.snakemake.*"]
  }

---------------------------
-- labeled searches file --
---------------------------

type SearchLabel = String -- TODO bytestring? newtype?

-- | We store them as Strings to keep the config simple, then compile in listTreePaths.
-- type SearchString = String

-- type LabeledSearches = [(SearchLabel, [SearchString])]

-- instance ToJSON LabeledSearches where toEncoding = genericToEncoding defaultOptions
-- instance FromJSON LabeledSearches where parseJSON = genericParseJSON defaultOptions

-- parseLabeledSearches :: FilePath -> IO (Either String LabeledSearches)
-- parseLabeledSearches = eitherDecodeFileStrict

data Search = Search
  { dirContainsPath       :: Maybe String
  , baseNameMatchesRegex  :: Maybe String
  , wholeNameMatchesRegex :: Maybe String
  }
  deriving (Read, Show, Generic)

instance ToJSON Search where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

instance FromJSON Search where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

type LabeledSearches = [(SearchLabel, [Search])]

parseLabeledSearches :: FilePath -> IO (Either String LabeledSearches)
parseLabeledSearches = eitherDecodeFileStrict

-------------------
-- search a tree --
-------------------

-- TODO variant that takes an OsPath and uses op2ns as before?
treeContainsPath :: HashTree a -> [Name] -> Bool
treeContainsPath tree names = isJust $ dropTo tree names

-- TODO write tests for this! it seems potentially a little off
dropTo :: HashTree a -> [Name] -> Maybe (HashTree a)
dropTo _ [] = Nothing
dropTo t@(Dir {nodeData=nd1, dirContents=cs}) (n:ns)
  = case filter (\t -> treeName t == n) cs of
      []  -> Nothing
      [c] -> if null ns then Just c else msum $ map (`dropTo` ns) cs
      _   -> Nothing
dropTo _ _ = Nothing

treeContainsHash :: HashTree a -> Hash -> Bool
treeContainsHash (Err {}) _ = False
treeContainsHash (Link {nodeData=nd1}) h2 = hash nd1 == h2
treeContainsHash (File {nodeData=nd1}) h2 = hash nd1 == h2
treeContainsHash (Dir  {nodeData=nd1, dirContents=cs}) h2
  | hash nd1 == h2 = True
  | otherwise = any (`treeContainsHash` h2) cs

----------------------
-- compile searches --
----------------------

-- | These are optimized for speed at the cost of not supporting capture groups.
-- They haven't been tested enough for me to be confident that's necessary though.
-- TODO would case sensitive be a better default? it does NOT seem faster so far
compileRegex :: String -> Regex
compileRegex = makeRegexOpts cOpt eOpt
  where
    cOpt = defaultCompOpt { caseSensitive = False, lastStarGreedy = False }
    eOpt = defaultExecOpt { captureGroups = False }

data CompiledSearch = CompiledSearch
  { cDirContainsPath       :: Maybe [Name]
  , cBaseNameMatchesRegex  :: Maybe Regex
  , cWholeNameMatchesRegex :: Maybe Regex
  }

type CompiledLabeledSearches = [(SearchLabel, [CompiledSearch])]

compileLabeledSearches :: LabeledSearches -> IO CompiledLabeledSearches
compileLabeledSearches [] = return []
compileLabeledSearches ((l, ss):lss) = do
  cs  <- mapM compile ss
  css <- compileLabeledSearches lss
  return $ (l, cs) : css
  where
    compile s = do
      ns <- case dirContainsPath s of
              Nothing -> return Nothing
              Just p  -> Just <$> fp2ns p
      return $ CompiledSearch
        { cDirContainsPath       = ns
        , cBaseNameMatchesRegex  = compileRegex <$> baseNameMatchesRegex s
        , cWholeNameMatchesRegex = compileRegex <$> wholeNameMatchesRegex s
        }
