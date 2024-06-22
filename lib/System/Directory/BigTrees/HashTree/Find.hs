{-# LANGUAGE ImpredicativeTypes #-}

module System.Directory.BigTrees.HashTree.Find where
  -- ( listTreePaths
  -- , pathMatches
  -- )
  -- where

import Control.Monad (when)
import qualified Data.ByteString.Char8 as B8
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Maybe (mapMaybe)
import System.Directory.BigTrees.Hash (Hash, prettyHash)
import System.Directory.BigTrees.HashLine (Depth (..), ModTime (..), NBytes (..), NNodes (..),
                                           TreeType (..), sepChar)
import System.Directory.BigTrees.HashTree.Base (HashTree (..), NodeData (..), sumNodes, treeModTime,
                                                treeNBytes, treeName, treeType)
import System.Directory.BigTrees.HashTree.Search (SearchConfig (..),
                                                  SearchLabel, Search2(..), LabeledSearch2, treeContainsPath)
import System.Directory.BigTrees.Name (Name(..), breadcrumbs2bs, fp2ns, n2bs)
import System.IO (hFlush, stdout)
import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString

-- import Debug.Trace

----------------
-- list paths --
----------------

{- We sort on filename here because 1) it's the only thing we can sort on
 - without keeping additional state, and 2) it makes it easy to property test
 - that `bigtrees find <path>` always matches `find <path>`.
 - TODO also consider excludeRegexes here? Or should they have been handled already?
 -}
listTreePaths :: SearchConfig -> String -> HashTree a -> IO [B8.ByteString]
listTreePaths cfg fmt tree = do
  cls <- compileLabeledSearch2 $ searchRegexes cfg
  return $ case mkLineMetaFormatter fmt of
    (Left  errMsg) -> error errMsg -- TODO anything to do besides die here?
    (Right fmtFn ) -> listTreePaths' cfg cls fmtFn (Depth 0) [] tree

{- Recursively render paths, passing a list of breadcrumbs.
 - Gotcha: breadcrumbs are in reverse order to make `cons`ing simple
 - TODO implement this via Foldable or Traversable instead?
 -}
listTreePaths' :: SearchConfig -> CompiledLabeledSearches -> FmtFn -> Depth -> [Name] -> HashTree a -> [B8.ByteString]
listTreePaths' cfg cls fmtFn (Depth d) ns t =
  let ns' = treeName t:ns

      recPaths = case t of
        (Dir {}) -> concat $ (flip map) (dirContents t) $
                      listTreePaths' cfg cls fmtFn (Depth $ d+1) ns'
        _        -> []

  in
     -- If no regexes, list everything.
     if null cls then
       let curPaths = ([pathLine fmtFn (Depth d) Nothing ns t | findKeepNode cfg (Depth d) t])
       in curPaths ++ recPaths

     -- If the current path matches we DO NOT need to search inside it, because
     -- we already have the one unique top-level match we want.
     -- else if pathMatches cls ns' then curPaths
     else if findKeepNode cfg (Depth d) t then
       case findLabelNode2 cls ns t of
         Nothing -> recPaths -- node matches other "keep" criteria but none of the regexes
         Just l  -> [pathLine fmtFn (Depth d) (Just l) ns t] -- has a labeled match

     -- If there are regexes but they don't match, keep looking.
     else recPaths

findKeepNode :: SearchConfig -> Depth -> HashTree a -> Bool
findKeepNode _ _ (Err {}) = False -- TODO is this how we should handle them?
findKeepNode cfg d t = and
  [ maybe True (d >=) $ minDepth cfg
  , maybe True (d <=) $ maxDepth cfg
  , maybe True (treeNBytes  t >=) $ minBytes cfg
  , maybe True (treeNBytes  t <=) $ maxBytes cfg
  , maybe True (sumNodes    t >=) $ minFiles cfg
  , maybe True (sumNodes    t <=) $ maxFiles cfg
  , maybe True (treeModTime t >=) $ minModtime cfg
  , maybe True (treeModTime t <=) $ maxModtime cfg
  , maybe True (treeType t `elem`) $ treeTypes cfg -- no need to save Dirs this time
  -- TODO finish regex conditions here
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
-- TODO have a distinction between filtering name and wholename?

-- | These are optimized for speed at the cost of not supporting capture groups.
-- They haven't been tested enough for me to be confident that's necessary though.
-- TODO would case sensitive be a better default? it does NOT seem faster so far
compileRegex :: String -> Regex
compileRegex = makeRegexOpts cOpt eOpt
  where
    cOpt = defaultCompOpt { caseSensitive = False, lastStarGreedy = False }
    eOpt = defaultExecOpt { captureGroups = False }

-- type LabeledRegexes = [(SearchLabel, [Regex])]

-- compileRegexes :: LabeledSearchStrings -> LabeledRegexes
-- compileRegexes []            = []
-- compileRegexes ((l, ss):lrs) = (l, map compileRegex ss) : compileRegexes lrs

-- pathMatches :: LabeledRegexes -> [Name] -> Bool
-- pathMatches [] _  = False -- TODO True? (since no searches = match everything)
-- pathMatches rs ns = flip any rs $ \r -> matchTest r $ breadcrumbs2bs ns

-- findLabelNode :: LabeledRegexes -> [Name] -> Maybe SearchLabel
-- findLabelNode [] _ = Nothing
-- findLabelNode ((l, rs):lrs) ns = if anyMatch then Just l else findLabelNode lrs ns
--   where
--     anyMatch = flip any rs $ \r -> matchTest r $ breadcrumbs2bs ns

data CompiledSearch2 = CompiledSearch2
  { cDirContainsPath       :: Maybe [Name]
  , cBaseNameMatchesRegex  :: Maybe Regex
  , cWholeNameMatchesRegex :: Maybe Regex
  }

type CompiledLabeledSearches = [(SearchLabel, [CompiledSearch2])]

compileLabeledSearch2 :: LabeledSearch2 -> IO CompiledLabeledSearches
compileLabeledSearch2 [] = return []
compileLabeledSearch2 ((l, ss):lss) = do
  cs  <- mapM compile ss
  css <- compileLabeledSearch2 lss
  return $ (l, cs) : css
  where
    compile s = do
      ns <- case dirContainsPath s of
              Nothing -> return Nothing
              Just p  -> Just <$> fp2ns p
      return $ CompiledSearch2
        { cDirContainsPath       = ns
        , cBaseNameMatchesRegex  = compileRegex <$> baseNameMatchesRegex s
        , cWholeNameMatchesRegex = compileRegex <$> wholeNameMatchesRegex s
        }

findLabelNode2 :: CompiledLabeledSearches -> [Name] -> HashTree a -> Maybe SearchLabel
findLabelNode2 []            _  _ = Nothing
findLabelNode2 ((l, cs):css) ns t = if anySearchMatches then Just l else findLabelNode2 css ns t
  where
    baseName  = n2bs $ treeName t
    wholeName = breadcrumbs2bs $ treeName t : ns
    anySearchMatches = any searchMatches cs
    searchMatches c = and
      [ fromMaybe True $ (treeContainsPath t) <$> cDirContainsPath c
      , fromMaybe True $ (\r -> matchTest r baseName) <$> cBaseNameMatchesRegex c
      , fromMaybe True $ (\r -> matchTest r wholeName) <$> cWholeNameMatchesRegex c
      ]

---------------------
-- format metadata --
---------------------

type FmtFn = forall a. Depth -> Maybe SearchLabel -> HashTree a -> B8.ByteString

-- TODO complain if nub is needed rather than silently fixing it?
matchingFmtFns :: String -> [FmtFn]
matchingFmtFns = mapMaybe (\c -> lookup c allFmtFns) . nub

-- TODO tabs instead of single spaces?
separate :: [B8.ByteString] -> B8.ByteString
separate = B8.intercalate $ B8.singleton sepChar

combineFmtFns :: [FmtFn] -> FmtFn
combineFmtFns fs d l t = separate $ map (\f -> f d l t) fs

allFmtFns :: [(Char, FmtFn)]
allFmtFns =
  [ ('t', \_ _ t -> B8.pack $ show $ treeType t)
  , ('h', \_ _ t -> prettyHash $ hash $ nodeData t)
  , ('d', \(Depth i) _ _ -> B8.pack $ show i)
  , ('m', \_ _ t -> B8.pack $ show $ (\(ModTime n) -> n) $ modTime $ nodeData t)
  , ('b', \_ _ t -> B8.pack $ show $ (\(NBytes n ) -> n) $ nBytes $ nodeData t)
  , ('f', \_ _ t -> B8.pack $ show $ (\(NNodes n ) -> n) $ sumNodes t) -- f for "files"
  , ('l', \_ mLabel _ -> case mLabel of
                       Nothing    -> error "no search label given, but it was specified in out-fmt"
                       Just label -> B8.pack label) -- TODO any sanitizing needed?
  ]

validFmtChars :: String
validFmtChars = map fst allFmtFns

{- | The overall "make formatter" function. Takes the metafmt description and
 - returns an error if it's invalid, or a function for formatting the metadata.
 - TODO return a list of bytestrings and let the caller handle intercalating?
 - TODO test that it throws exceptions on invalid formats
 -}
mkLineMetaFormatter :: String -> Either String FmtFn
mkLineMetaFormatter cs =
  let bad = filter (not . flip elem validFmtChars) cs
  in if not (null bad)
       then Left  $ "Invalid metadata format char '" ++ bad ++ "' in " ++ show cs
       else Right $ combineFmtFns $ matchingFmtFns cs
