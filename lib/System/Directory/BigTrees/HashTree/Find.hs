module System.Directory.BigTrees.HashTree.Find
  ( printTreePaths
  , pathLine
  , mkLineMetaFormatter
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
import Data.List (nub)

{- We sort on filename here because 1) it's the only thing we can sort on
 - without keeping additional state, and 2) it makes it easy to property test
 - that `bigtrees find <path>` always matches `find <path> | sort`.
 -}
-- TODO pass which metadata options to print here without a Config
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

-- TODO where should this live?
treeType :: HashTree a -> Char
treeType (File {}) = 'F'
treeType (Dir  {}) = 'D'

-- TODO complain if nub is needed rather than silently fixing it?
matchingFmtFns :: String -> [IndentLevel -> HashTree a -> B8.ByteString]
matchingFmtFns = catMaybes . map (\c -> lookup c allFmtFns) . nub

-- TODO B8.intercalate (B8.singleton '\t') everywhere instead of unwords?
combineFmtFns
  :: [IndentLevel -> HashTree a -> B8.ByteString]
  -> (IndentLevel -> HashTree a -> B8.ByteString)
combineFmtFns fs i t = B8.unwords $ map (\f -> f i t) fs

allFmtFns :: [(Char, IndentLevel -> HashTree a -> B8.ByteString)]
allFmtFns =
  [ ('t', \_ t -> B8.singleton $ treeType t)
  , ('h', \_ t -> prettyHash $ hash t)
  , ('i', \(IndentLevel i) _ -> B8.pack $ show i)
  ]

validFmtChars :: String
validFmtChars = "thi"

-- The overall "make formatter" function. Takes the metafmt description and
-- returns an error if it's invalid, or a function for formatting the metadata.
mkLineMetaFormatter
  :: String
  -> Either String (IndentLevel -> HashTree a -> B8.ByteString)
mkLineMetaFormatter cs =
  let invalid = filter (not . flip elem validFmtChars) cs
  in if not (null invalid)
       then Left  $ "Invalid metafmt: \"" ++ invalid ++ "\""
       else Right $ combineFmtFns $ matchingFmtFns cs
