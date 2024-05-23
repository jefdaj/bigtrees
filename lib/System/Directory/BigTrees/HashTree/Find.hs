module System.Directory.BigTrees.HashTree.Find
  ( printTreePaths
  , pathLine
  )
  where

-- import Data.List (sortOn)
import System.Directory.BigTrees.HashTree.Base (HashTree (..))
import System.Directory.BigTrees.Name (Name, breadcrumbs2fp)
import System.Directory.BigTrees.Hash (Hash, prettyHash)
import System.Directory.BigTrees.HashLine (IndentLevel(..), TreeType(..))
-- import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Char8 as B8
import System.IO (hFlush, stdout) -- TODO open stdout in binary mode?

{- We sort on filename here because 1) it's the only thing we can sort on
 - without keeping additional state, and 2) it makes it easy to property test
 - that `bigtrees find <path>` always matches `find <path> | sort`.
 -}
printTreePaths :: HashTree a -> IO ()
printTreePaths = printTreePaths' []

{- Recursively print paths, passing a list of breadcrumbs.
 - A couple gotchas:
 - * breadcrumbs are in reverse order to make `cons`ing simple
 - * have to print subtree paths before the main dir to maintain streaming
 -   (otherwise the entire tree has to be held in memory)
 -}
printTreePaths' :: [Name] -> HashTree a -> IO ()
printTreePaths' ns t = do
  let ns' = name t:ns
  case t of
    (Dir {}) -> mapM_ (printTreePaths' ns') (contents t)
    _        -> return ()
  let tt = case t of
             (File {}) -> F
             (Dir  {}) -> D
  B8.putStrLn $ pathLine
    (Just tt)
    (Just $ IndentLevel $ length ns)
    (Just $ hash t)
    ns'
  hFlush stdout -- TODO maybe not?

pathLine
  :: Maybe TreeType
  -> Maybe IndentLevel
  -> Maybe Hash
  -> [Name]
  -> B8.ByteString
pathLine mt mi mh ns = B8.unwords $ meta ++ [path] -- TODO tab separate
  where
    path = B8.pack $ breadcrumbs2fp ns
    meta = catMaybes
      [ (B8.pack . show) <$> mt
      , (B8.pack . (\(IndentLevel n) -> show n)) <$> mi
      ,  prettyHash <$> mh
      ]
