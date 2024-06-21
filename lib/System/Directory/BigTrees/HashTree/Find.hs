{-# LANGUAGE ImpredicativeTypes #-}

module System.Directory.BigTrees.HashTree.Find
  ( listTreePaths
  , Filter(..)
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
                                                treeType)
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
 - that `bigtrees find <path>` always matches `find <path> | sort`.
 - TODO also consider excludeRegexes here? Or should they have been handled already?
 -}
listTreePaths :: SearchConfig -> String -> HashTree a -> [B8.ByteString]
listTreePaths cfg fmt =
  let fs = if null (searchRegexes cfg) then [Anything] else map FilterRegex $ searchRegexes cfg
  in case mkLineMetaFormatter fmt of
       (Left  errMsg) -> error errMsg -- TODO anything to do besides die here?
       (Right fmtFn ) -> listTreePaths' fs fmtFn (Depth 0) []

{- Recursively render paths, passing a list of breadcrumbs.
 - Gotcha: breadcrumbs are in reverse order to make `cons`ing simple
 - TODO implement this via Foldable or Traversable instead?
 -}
listTreePaths' :: [Filter] -> FmtFn -> Depth -> [Name] -> HashTree a -> [B8.ByteString]
listTreePaths' fs fmtFn (Depth i) ns t =
  let ns' = treeName t:ns
  -- If the current path matches we DO NOT need to search inside it, because
  -- we only want one unique top-level match.
  in if pathMatches fs ns'
       then pathLine fmtFn (Depth i) ns t : [] -- : recPaths
       else case t of
         (Dir {}) -> concat $ (flip map) (dirContents t) $
                       listTreePaths' fs fmtFn (Depth $ i+1) ns'
         _        -> []

pathLine :: FmtFn -> Depth -> [Name] -> HashTree a -> B8.ByteString
pathLine fmtFn i ns t = separate $ filter (not . B8.null) [meta, path]
  where
    meta = fmtFn i t
    path = breadcrumbs2bs $ treeName t:ns -- TODO ns already includes name t?

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
  [ ('t', \_ t -> B8.singleton $ treeType t)
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

------------------
-- filter paths --
------------------

-- TODO have a distinction between filtering paths and filtering tree nNodes?
-- TODO have a distinction between filtering name and wholename?

-- TODO does the string type of the regex matter? String is probably fine right?
data Filter
  = Anything -- TODO is this useful?
  | FilterRegex String
  deriving (Read, Show)

pathMatches :: [Filter] -> [Name] -> Bool
pathMatches []                    _  = False -- TODO is that right?
pathMatches (Anything:_)          _  = True
pathMatches ((FilterRegex re):fs) ns = (breadcrumbs2bs ns) =~ re || pathMatches fs ns
