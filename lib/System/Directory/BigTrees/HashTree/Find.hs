{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}

module System.Directory.BigTrees.HashTree.Find where
  -- ( listTreePaths
  -- , pathMatches
  -- )
  -- where

import Control.Monad (forM, (>=>))
import Control.Monad.ST.Strict (ST, runST)
import qualified Data.ByteString.Char8 as B8
import Data.List (nub)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Directory.BigTrees.Hash (prettyHash)
import System.Directory.BigTrees.HashLine (Depth (..), ModTime (..), NBytes (..), NNodes (..),
                                           sepChar)
import System.Directory.BigTrees.HashSet (HashSet, hashSetFromList, readHashList,
                                          setContainsHash)
import System.Directory.BigTrees.HashTree.Base (HashTree (..), NodeData (..), sortContentsByName,
                                                treeHash, treeModTime, treeNBytes, treeNNodes,
                                                treeName, treeType)
import System.Directory.BigTrees.HashTree.Search (CompiledLabeledSearches, CompiledSearch (..),
                                                  LabeledSearches, Search (..), SearchConfig (..),
                                                  SearchLabel, compileLabeledSearches,
                                                  treeContainsPath)
import System.Directory.BigTrees.Logging (LogCfg, LogLevel (..), addLogContext, die, logUnsafe)
import System.Directory.BigTrees.Name (Name (..), breadcrumbs2bs, n2bs)
-- import System.IO (hFlush, stdout)
import System.OsPath (encodeFS)
import Text.Regex.TDFA
-- import Text.Regex.TDFA.ByteString

----------------
-- list paths --
----------------

{- We sort on filename here because 1) it's the only thing we can sort on
 - without keeping additional state, and 2) it makes it easy to property test
 - that `bigtrees find <path>` always matches `find <path>`. This is a lossy
 - function because it decodes the paths, making them no longer necessarily
 - comparable across systems.
 - TODO also consider hashExcludeRegexes here? Or should they have been handled already?
 -}
listTreePaths :: SearchConfig -> LogCfg -> String -> HashTree a -> IO [B8.ByteString]
listTreePaths cfg lCfg fmt tree = do
  cls <- compileLabeledSearches $ searches cfg
  -- TODO is it a problem allocating memory for this list in addition to the hashset?
  eLists <- forM (excludeSetPaths cfg) (encodeFS >=> readHashList lCfg)
  return $ case mkLineMetaFormatter lCfg fmt of
    (Left  msg   ) -> die (addLogContext lCfg "listTreePaths") $ B8.pack msg
    (Right fmtFn ) -> runST $ do
      eSet <- hashSetFromList $ concat eLists
      listTreePaths' cfg lCfg cls eSet fmtFn (Depth 0) [] tree

{- Recursively render paths, passing a list of breadcrumbs.
 - Gotcha: breadcrumbs are in reverse order to make `cons`ing simple
 - TODO implement this via Foldable or Traversable instead?
 -}
listTreePaths'
  :: SearchConfig            -- ^ Main search config
  -> LogCfg                  -- ^ logging config
  -> CompiledLabeledSearches -- ^ labeled searches
  -> HashSet s               -- ^ Hashes to exclude (may be empty)
  -> FmtFn                   -- ^ Path formatting function
  -> Depth                   -- ^ Depth of the tree for filtering min/max
  -> [Name]                  -- ^ Breadcrummbs/anchor to prefix paths with
  -> HashTree a              -- ^ The tree to list paths from
  -> ST s [B8.ByteString]
listTreePaths' cfg lCfg cls eSet fmtFn (Depth d) ns t = do
  let ns' = treeName t:ns

  recPaths <- case t of

        (Dir {}) ->
          fmap concat $ forM (sortContentsByName $ dirContents t) $ \t' ->
            listTreePaths' cfg lCfg cls eSet fmtFn (Depth $ d+1) ns' t'

        _        -> return []

  keepNode <- findKeepNode cfg lCfg eSet (Depth d) t

  return $
     -- If no regexes, list everything.
     if null cls then
       let curPaths = ([pathLine fmtFn (Depth d) Nothing ns t | keepNode])
       in curPaths ++ recPaths

     -- If the current path matches we DO NOT need to search inside it, because
     -- we already have the one unique top-level match we want.
     else if keepNode then
       case findLabelNode cls ns t of
         Nothing -> recPaths -- node matches other "keep" criteria but none of the regexes
         Just l  -> [pathLine fmtFn (Depth d) (Just l) ns t] -- has a labeled match

     -- If there are regexes but they don't match, keep looking.
     else recPaths

findKeepNode :: SearchConfig -> LogCfg -> HashSet s -> Depth -> HashTree a -> ST s Bool
findKeepNode _ _ _ _ (Err {}) = return False -- TODO is this how we should handle them?
findKeepNode cfg lCfg eSet d t = do
  excludeHash <- setContainsHash eSet $ treeHash t
  let excludeHash' = if excludeHash
                       then logUnsafe (addLogContext lCfg "findKeepNode") DebugL
                              ("find exclude hash " <> prettyHash (treeHash t) <>
                               ": " <> n2bs (treeName t))
                              excludeHash
                       else excludeHash
  return $ and
    [ maybe True (d >=) $ minDepth cfg
    , maybe True (d <=) $ maxDepth cfg
    , maybe True (treeNBytes  t >=) $ minBytes cfg
    , maybe True (treeNBytes  t <=) $ maxBytes cfg
    , maybe True (treeNNodes  t >=) $ minFiles cfg
    , maybe True (treeNNodes  t <=) $ maxFiles cfg
    , maybe True (treeModTime t >=) $ minModtime cfg
    , maybe True (treeModTime t <=) $ maxModtime cfg
    , maybe True (treeType t `elem`) $ treeTypes cfg -- no need to save Dirs this time
    , not excludeHash'
    -- TODO finish regex conditions here?
    ]

-- | A SearchLabel should always be available, unless there are no regex searches at all.
-- When --search-regex is used on the CLI, the label defaults to "unlabeled-search".
pathLine :: FmtFn -> Depth -> Maybe SearchLabel -> [Name] -> HashTree a -> B8.ByteString
pathLine fmtFn d ml ns t = separate $ filter (not . B8.null) [meta, path]
  where
    meta = fmtFn d ml t
    path = breadcrumbs2bs $ treeName t:ns -- TODO ns already includes name t?

------------------
-- filter paths --
------------------

-- TODO have a distinction between filtering paths and filtering tree nNodes?

findLabelNode :: CompiledLabeledSearches -> [Name] -> HashTree a -> Maybe SearchLabel
findLabelNode []            _  _ = Nothing
findLabelNode ((l, cs):css) ns t = if anySearchMatches then Just l else findLabelNode css ns t
  where
    baseName  = n2bs $ treeName t
    wholeName = breadcrumbs2bs $ treeName t : ns
    anySearchMatches = any searchMatches cs
    searchMatches c = and
      [ fromMaybe True $ (treeContainsPath t      ) <$> cDirContainsPath c
      , fromMaybe True $ (flip matchTest baseName ) <$> cBaseNameMatchesRegex c
      , fromMaybe True $ (flip matchTest wholeName) <$> cWholeNameMatchesRegex c
      ]


---------------------
-- format metadata --
---------------------

type FmtFn = forall a. Depth -> Maybe SearchLabel -> HashTree a -> B8.ByteString

-- TODO complain if nub is needed rather than silently fixing it?
matchingFmtFns :: LogCfg -> String -> [FmtFn]
matchingFmtFns lCfg = mapMaybe (\c -> lookup c $ allFmtFns lCfg) . nub

-- TODO tabs instead of single spaces?
separate :: [B8.ByteString] -> B8.ByteString
separate = B8.intercalate $ B8.singleton sepChar

combineFmtFns :: [FmtFn] -> FmtFn
combineFmtFns fs d l t = separate $ map (\f -> f d l t) fs

allFmtFns :: LogCfg -> [(Char, FmtFn)]
allFmtFns lCfg =
  [ ('t', \_ _ t -> B8.pack $ show $ treeType t)
  , ('h', \_ _ t -> prettyHash $ hash $ nodeData t)
  , ('d', \(Depth i) _ _ -> B8.pack $ show i)
  , ('m', \_ _ t -> B8.pack $ show $ (\(ModTime n) -> n) $ modTime $ nodeData t)
  , ('b', \_ _ t -> B8.pack $ show $ (\(NBytes n ) -> n) $ nBytes $ nodeData t)
  , ('f', \_ _ t -> B8.pack $ show $ (\(NNodes n ) -> n) $ treeNNodes t) -- f for "files"
  , ('l', \_ mLabel _ -> case mLabel of
                       Nothing    -> die (addLogContext lCfg "allFmtFns") "no search label given, but it was specified in out-fmt"
                       Just label -> B8.pack label) -- TODO any sanitizing needed?
  ]

validFmtChars :: LogCfg -> String
validFmtChars lCfg = map fst $ allFmtFns lCfg

{- | The overall "make formatter" function. Takes the metafmt description and
 - returns an error if it's invalid, or a function for formatting the metadata.
 - TODO return a list of bytestrings and let the caller handle intercalating?
 - TODO test that it throws exceptions on invalid formats
 -}
mkLineMetaFormatter :: LogCfg -> String -> Either String FmtFn
mkLineMetaFormatter lCfg cs =
  let bad = filter (not . flip elem (validFmtChars lCfg)) cs
  in if not (null bad)
       then Left  $ "Invalid metadata format char '" ++ bad ++ "' in " ++ show cs
       else Right $ combineFmtFns $ matchingFmtFns lCfg cs
