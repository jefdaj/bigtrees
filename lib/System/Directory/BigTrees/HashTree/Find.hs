module System.Directory.BigTrees.HashTree.Find
  ( printTreePaths
  , printPath
  )
  where

import Data.List (sortOn)
import System.Directory.BigTrees.HashTree.Base (HashTree (..))
import System.Directory.BigTrees.Name (Name, n2fp)
import System.FilePath ((</>))

{- We sort on filename here because 1) it's the only thing we can sort on
 - without keeping additional state, and 2) it makes it easy to property test
 - that `bigtrees find <path>` always matches `find <path> | sort`.
 -}
printTreePaths :: HashTree a -> IO ()
printTreePaths = printTreePaths' []

{- Recursively print paths, passing a list of breadcrumbs. Note that the
 - breadcrumbs are in reverse order to make `cons`ing simple.
 -}
printTreePaths' :: [Name] -> HashTree a -> IO ()
printTreePaths' ns t = do
  let ns' = name t:ns
  printPath ns'
  case t of
    (Dir {}) -> mapM_ (printTreePaths' ns') (sortOn name $ contents t)
    _        -> return ()

-- Note that the names start in reverse order, then we flip them before printing.
-- TODO what's the best fold fn to use here?
printPath :: [Name] -> IO ()
printPath = putStrLn . foldl1 (flip (</>)) . map n2fp

-- TODO prop_printTreePaths_matches_unix_find_and_sort
