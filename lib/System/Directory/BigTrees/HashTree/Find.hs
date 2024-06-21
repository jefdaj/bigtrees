{-# LANGUAGE ImpredicativeTypes #-}

module System.Directory.BigTrees.HashTree.Find
  ( listTreePaths
  , pathMatches
  )
  where

import Control.Monad (when)
import qualified Data.ByteString.Char8 as B8
import Data.List (nub)
import Data.Maybe (mapMaybe)
import System.Directory.BigTrees.Hash (Hash, prettyHash)
import System.Directory.BigTrees.HashLine (Depth (..), ModTime (..), NBytes (..), NNodes (..),
                                           TreeType (..), sepChar)
import System.Directory.BigTrees.HashTree.Base (HashTree (..), NodeData (..), sumNodes, treeName,
                                                treeType, treeModTime, treeNBytes)
import System.Directory.BigTrees.HashTree.Search (SearchConfig(..))
import System.Directory.BigTrees.Name (Name, breadcrumbs2bs)
import System.IO (hFlush, stdout)
import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString

import Debug.Trace

----------------
-- list paths --
----------------

{- We sort on filename here because 1) it's the only thing we can sort on
 - without keeping additional state, and 2) it makes it easy to property test
 - that `bigtrees find <path>` always matches `find <path>`.
 - TODO also consider excludeRegexes here? Or should they have been handled already?
 -}
listTreePaths :: SearchConfig -> String -> HashTree a -> [B8.ByteString]
listTreePaths cfg fmt =
  let rs = map compileRegex $ searchRegexes cfg
  in case mkLineMetaFormatter fmt of
       (Left  errMsg) -> error errMsg -- TODO anything to do besides die here?
       (Right fmtFn ) -> listTreePaths' cfg rs fmtFn (Depth 0) []

{- Recursively render paths, passing a list of breadcrumbs.
 - Gotcha: breadcrumbs are in reverse order to make `cons`ing simple
 - TODO implement this via Foldable or Traversable instead?
 -}
listTreePaths' :: SearchConfig -> [Regex] -> FmtFn -> Depth -> [Name] -> HashTree a -> [B8.ByteString]
listTreePaths' cfg rs fmtFn (Depth d) ns t =
  let ns' = treeName t:ns

      curPaths = if findKeepNode cfg (Depth d) t
                   then [pathLine fmtFn (Depth d) ns t]
                   else []

      recPaths = case t of
        (Dir {}) -> concat $ (flip map) (dirContents t) $
                      listTreePaths' cfg rs fmtFn (Depth $ d+1) ns'
        _        -> []

  in
     -- If no regexes, list everything.
     if null rs then curPaths ++ recPaths

     -- If the current path matches we DO NOT need to search inside it, because
     -- we already have the one unique top-level match we want.
     else if pathMatches rs ns' then curPaths

     -- If there are regexes but they don't match, keep looking.
     else recPaths

findKeepNode :: SearchConfig -> Depth -> HashTree a -> Bool
findKeepNode _ _ (Err {}) = False -- TODO is this how we should handle them?
findKeepNode cfg d t = all id
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

pathLine :: FmtFn -> Depth -> [Name] -> HashTree a -> B8.ByteString
pathLine fmtFn i ns t = separate $ filter (not . B8.null) [meta, path]
  where
    meta = fmtFn i t
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

pathMatches :: [Regex] -> [Name] -> Bool
pathMatches [] _  = False -- TODO remove?
pathMatches rs ns = flip any rs $ \r -> matchTest r $ breadcrumbs2bs ns

---------------------
-- format metadata --
---------------------

type FmtFn = forall a. Depth -> HashTree a -> B8.ByteString

-- TODO complain if nub is needed rather than silently fixing it?
matchingFmtFns :: String -> [FmtFn]
matchingFmtFns = mapMaybe (\c -> lookup c allFmtFns) . nub

-- TODO tabs instead of single spaces?
separate :: [B8.ByteString] -> B8.ByteString
separate = B8.intercalate $ B8.singleton sepChar

combineFmtFns :: [FmtFn] -> FmtFn
combineFmtFns fs i t = separate $ map (\f -> f i t) fs

allFmtFns :: [(Char, FmtFn)]
allFmtFns =
  [ ('t', \_ t -> B8.pack $ show $ treeType t)
  , ('h', \_ t -> prettyHash $ hash $ nodeData t)
  , ('d', \(Depth i) _ -> B8.pack $ show i)
  , ('m', \_ t -> B8.pack $ show $ (\(ModTime n) -> n) $ modTime $ nodeData t)
  , ('b', \_ t -> B8.pack $ show $ (\(NBytes n ) -> n) $ nBytes $ nodeData t)
  , ('f', \_ t -> B8.pack $ show $ (\(NNodes n ) -> n) $ sumNodes t) -- f for "files"
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
