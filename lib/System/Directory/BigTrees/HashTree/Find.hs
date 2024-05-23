module System.Directory.BigTrees.HashTree.Find
  ( printTreePaths
  , printPath
  )
  where

import Data.List (sortOn)
import System.Directory.BigTrees.HashTree.Base (HashTree (..))
import System.Directory.BigTrees.Name (Name, n2fp, breadcrumbs2fp)

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
    -- TODO why doesn't this sort order match the one in Cmd.Find?
    (Dir {}) -> mapM_ (printTreePaths' ns') (sortOn name $ contents t)
    _        -> return ()

-- TODO what's the best fold fn to use here?
-- TODO take a tree + format here too and add metadata to the printout
-- TODO except this should ideally match the system used for hashlines too...
--      simplest way to start: fixed order, each thing controlled by a flag
-- TODO factor some of this out into ns2fp?
printPath :: [Name] -> IO ()
printPath = putStrLn . breadcrumbs2fp
