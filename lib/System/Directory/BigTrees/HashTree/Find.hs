{-# LANGUAGE ImpredicativeTypes #-}

module System.Directory.BigTrees.HashTree.Find
  ( listTreePaths
  , Filter(..)  -- TODO remove from exports?
  , pathMatches -- TODO remove from exports?
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
import System.Directory.BigTrees.Name (Name, breadcrumbs2bs)
import System.IO (hFlush, stdout)
import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString


-------------------------------------
-- list paths, but don't print yet --
-------------------------------------

-- TODO should this be implemented in terms of a Foldable/Traversable instance?
-- TODO don't try to make it efficient before doing the reverse read thing?

listTreePaths :: Maybe String -> String -> HashTree a -> [B8.ByteString]
listTreePaths mRegex fmt =
  let fExpr = maybe Anything FilterRegex mRegex
  in case mkLineMetaFormatter fmt of
       (Left  errMsg) -> error errMsg -- TODO anything to do besides die here?
       (Right fmtFn ) -> listTreePaths' fExpr fmtFn (Depth 0) []

listTreePaths' :: Filter -> FmtFn -> Depth -> [Name] -> HashTree a -> [B8.ByteString]
listTreePaths' fExpr fmtFn (Depth i) ns t =
  let ns' = treeName t:ns
  -- If the current path matches we DO NOT need to search inside it, because
  -- we only want one unique top-level match.
  in if pathMatches fExpr ns'
       then pathLine fmtFn (Depth i) ns t : [] -- : recPaths
       else case t of
         (Dir {}) -> concat $ (flip map) (dirContents t) $
                       listTreePaths' fExpr fmtFn (Depth $ i+1) ns'
         _        -> []


-----------------
-- print paths --
-----------------

{- We sort on filename here because 1) it's the only thing we can sort on
 - without keeping additional state, and 2) it makes it easy to property test
 - that `bigtrees find <path>` always matches `find <path> | sort`.
 -}
-- TODO pass which metadata options to print here without a Config
printTreePaths :: Maybe String -> String -> HashTree a -> IO ()
printTreePaths mRegex fmt =
  let fExpr = maybe Anything FilterRegex mRegex
  in case mkLineMetaFormatter fmt of
    Left  errMsg -> error errMsg -- TODO anything to do besides die here?
    Right fmtFn  -> printTreePaths' fExpr fmtFn (Depth 0) []

{- Recursively print paths, passing a list of breadcrumbs.
 - A couple gotchas:
 - * breadcrumbs are in reverse order to make `cons`ing simple
 - * have to print main path before subtrees if any.... except we won't print
 -   subtrees in that case
 - TODO implement this via Foldable or Traversable instead?
 -}
printTreePaths' :: Filter -> FmtFn -> Depth -> [Name] -> HashTree a -> IO ()
printTreePaths' fExpr fmtFn (Depth i) ns t = do
  let ns' = treeName t:ns
  if pathMatches fExpr ns' then do
    B8.putStrLn $ pathLine fmtFn (Depth i) ns t
    hFlush stdout -- TODO maybe not?
  -- We actually want to skip this if the current path matches, because we just
  -- want one unique line for the top level of each match.
  else case t of
    (Dir {}) -> mapM_ (printTreePaths' fExpr fmtFn (Depth $ i+1) ns') (dirContents t)
    _        -> return ()

pathLine :: FmtFn -> Depth -> [Name] -> HashTree a -> B8.ByteString
pathLine fmtFn i ns t = separate $ filter (not . B8.null) [meta, path]
  where
    meta = fmtFn i t
    path = breadcrumbs2bs $ treeName t:ns -- TODO ns already includes name t?

---------------------
-- format metadata --
---------------------

-- TODO is the type variable a valid here?
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

-- The overall "make formatter" function. Takes the metafmt description and
-- returns an error if it's invalid, or a function for formatting the metadata.
-- TODO return a list of bytestrings and let the caller handle intercalating?
mkLineMetaFormatter :: String -> Either String FmtFn
mkLineMetaFormatter cs =
  let bad = filter (not . flip elem validFmtChars) cs
  in if not (null bad)
       then Left  $ "Invalid metadata format char '" ++ bad ++ "' in " ++ show cs
       else Right $ combineFmtFns $ matchingFmtFns cs

-- TODO test that ^ throws exceptions on invalid formats

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

pathMatches :: Filter -> [Name] -> Bool
pathMatches Anything _          = True
pathMatches (FilterRegex re) ns = (breadcrumbs2bs ns) =~ re
