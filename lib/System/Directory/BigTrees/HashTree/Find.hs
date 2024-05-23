{-# LANGUAGE ImpredicativeTypes #-}

module System.Directory.BigTrees.HashTree.Find
  ( printTreePaths
  )
  where

import qualified Data.ByteString.Char8 as B8
import Data.List (nub)
import Data.Maybe (mapMaybe)
import System.Directory.BigTrees.Hash (Hash, prettyHash)
import System.Directory.BigTrees.HashLine (IndentLevel (..), TreeType (..))
import System.Directory.BigTrees.HashTree.Base (HashTree (..))
import System.Directory.BigTrees.Name (Name, breadcrumbs2fp)
import System.IO (hFlush, stdout)

{- We sort on filename here because 1) it's the only thing we can sort on
 - without keeping additional state, and 2) it makes it easy to property test
 - that `bigtrees find <path>` always matches `find <path> | sort`.
 -}
-- TODO pass which metadata options to print here without a Config
printTreePaths :: String -> HashTree a -> IO ()
printTreePaths fmt =
  case mkLineMetaFormatter fmt of
    Left  errMsg -> error errMsg -- TODO anything to do besides die here?
    Right fmtFn  -> printTreePaths' fmtFn (IndentLevel 0) []

{- Recursively print paths, passing a list of breadcrumbs.
 - A couple gotchas:
 - * breadcrumbs are in reverse order to make `cons`ing simple
 - * have to print subtree paths before the main dir to maintain streaming
 -   (otherwise the entire tree has to be held in memory)
 -}
printTreePaths' :: FmtFn -> IndentLevel -> [Name] -> HashTree a -> IO ()
printTreePaths' fmtFn (IndentLevel i) ns t = do
  let ns' = name t:ns
      tt  = treeType t
  case t of
    (Dir {}) -> mapM_ (printTreePaths' fmtFn (IndentLevel $ i+1) (name t:ns)) (contents t)
    _        -> return ()
  B8.putStrLn $ pathLine fmtFn (IndentLevel i) ns t
  hFlush stdout -- TODO maybe not?

pathLine :: FmtFn -> IndentLevel -> [Name] -> HashTree a -> B8.ByteString
pathLine fmtFn i ns t = separate $ filter (not . B8.null) [meta, path]
  where
    meta = fmtFn i t
    path = B8.pack $ breadcrumbs2fp $ name t:ns -- TODO ns already includes name t?

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
  ]

validFmtChars :: String
validFmtChars = "thi"

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
