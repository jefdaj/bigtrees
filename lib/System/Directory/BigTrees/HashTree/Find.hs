{-# LANGUAGE ImpredicativeTypes #-}

module System.Directory.BigTrees.HashTree.Find
  ( printTreePaths
  , Filter(..)  -- TODO remove from exports?
  , pathMatches -- TODO remove from exports?
  )
  where

import Control.Monad (when)
import qualified Data.ByteString.Char8 as B8
import Data.List (nub)
import Data.Maybe (mapMaybe)
import System.Directory.BigTrees.Hash (Hash, prettyHash)
import System.Directory.BigTrees.HashLine (IndentLevel (..), TreeType (..), ModTime(..), Size(..))
import System.Directory.BigTrees.HashTree.Base (HashTree (..))
import System.Directory.BigTrees.Name (Name, breadcrumbs2fp)
import System.IO (hFlush, stdout)
import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString

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
    Right fmtFn  -> printTreePaths' fExpr fmtFn (IndentLevel 0) []

{- Recursively print paths, passing a list of breadcrumbs.
 - A couple gotchas:
 - * breadcrumbs are in reverse order to make `cons`ing simple
 - * have to print subtree paths before the main dir to maintain streaming
 -   (otherwise the entire tree has to be held in memory)
 -}
printTreePaths' :: Filter -> FmtFn -> IndentLevel -> [Name] -> HashTree a -> IO ()
printTreePaths' fExpr fmtFn (IndentLevel i) ns t = do
  let ns' = name t:ns
      tt  = treeType t
  case t of
    (Dir {}) -> mapM_ (printTreePaths' fExpr fmtFn (IndentLevel $ i+1) ns') (contents t)
    _        -> return ()
  when (pathMatches fExpr ns') $
    B8.putStrLn $ pathLine fmtFn (IndentLevel i) ns t
  hFlush stdout -- TODO maybe not?

pathLine :: FmtFn -> IndentLevel -> [Name] -> HashTree a -> B8.ByteString
pathLine fmtFn i ns t = separate $ filter (not . B8.null) [meta, path]
  where
    meta = fmtFn i t
    path = B8.pack $ breadcrumbs2fp $ name t:ns -- TODO ns already includes name t?

---------------------
-- format metadata --
---------------------

-- TODO where should this live?
treeType :: HashTree a -> Char
treeType (File {}) = 'F'
treeType (Dir  {}) = 'D'

-- TODO is the type variable a valid here?
type FmtFn = forall a. IndentLevel -> HashTree a -> B8.ByteString

-- TODO complain if nub is needed rather than silently fixing it?
matchingFmtFns :: String -> [FmtFn]
matchingFmtFns = mapMaybe (\c -> lookup c allFmtFns) . nub

-- TODO tabs instead of single spaces?
separate :: [B8.ByteString] -> B8.ByteString
separate = B8.intercalate $ B8.singleton ' '

combineFmtFns :: [FmtFn] -> FmtFn
combineFmtFns fs i t = separate $ map (\f -> f i t) fs

allFmtFns :: [(Char, FmtFn)]
allFmtFns =
  [ ('t', \_ t -> B8.singleton $ treeType t)
  , ('h', \_ t -> prettyHash $ hash t)
  , ('i', \(IndentLevel i) _ -> B8.pack $ show i)
  , ('m', \_ t -> B8.pack $ show $ (\(ModTime n) -> n) $ modTime t)
  , ('s', \_ t -> B8.pack $ show $ (\(Size n) -> n) $ size t)
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

-- TODO have a distinction between filtering paths and filtering tree nodes?
-- TODO have a distinction between filtering name and wholename?

-- TODO does the string type of the regex matter? String is probably fine right?
data Filter
  = Anything -- TODO is this useful?
  | FilterRegex String
  deriving (Read, Show)

pathMatches :: Filter -> [Name] -> Bool
pathMatches Anything _          = True
pathMatches (FilterRegex re) ns = (breadcrumbs2fp ns) =~ re
