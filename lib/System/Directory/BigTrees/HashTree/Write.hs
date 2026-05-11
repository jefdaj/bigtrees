{-# LANGUAGE OverloadedStrings #-}

module System.Directory.BigTrees.HashTree.Write where

-- import qualified Control.Concurrent.Thread.Delay as D
import Control.Monad (unless, when)
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (isNothing)
import System.Directory.BigTrees.HashLine (Depth (Depth), HashLine (..), TreeType (..),
                                           prettyLine)
import System.Directory.BigTrees.HashTree.Base (HashTree (..), NodeData (..), TestTree, renameRoot,
                                                sortContentsByName)
import System.Directory.BigTrees.HashTree.Search (SearchConfig (..))
import System.Directory.BigTrees.HeadFoot (hWriteFooter, hWriteHeader)
import System.Directory.BigTrees.Logging (LogCfg, addLogContext, die)
import System.Directory.BigTrees.Name (Name (..), unName)
import qualified System.Directory.OsPath as SDO
import qualified System.File.OsPath as SFO
import System.IO (Handle, IOMode (..), hFlush, stdout)
import System.OsPath (OsPath, decodeFS, takeBaseName, takeDirectory, (</>))

-- import Debug.Trace

-- TODO can Foldable or Traversable simplify these?
-- TODO need to handle unicode here?
-- TODO does map evaluation influence memory usage?
-- TODO create a single ByteString rather than a list for compression?
serializeTree :: LogCfg -> HashTree a -> [B8.ByteString]
serializeTree lCfg = map (prettyLine Nothing) . flattenTree lCfg

-- TODO remove and make this a special case of WriteTree? or vice versa?
printTree :: LogCfg -> HashTree a -> IO ()
printTree lCfg = mapM_ printLine . flattenTree lCfg
  where
    -- TODO don't flush every line
    printLine l = putStrLn (B8.unpack $ prettyLine Nothing l) >> hFlush stdout

-- this uses a handle for streaming output, which turns out to be important for memory usage
-- TODO rename writeHashes? this is a confusing way to say that
-- TODO how much of the config should live in the library vs the app, if we're writing it?
writeTree :: SearchConfig -> LogCfg -> OsPath -> HashTree a -> IO ()
writeTree cfg lCfg path tree = SFO.withFile path WriteMode $ \h -> hWriteTree cfg lCfg h tree

-- TODO excludes type alias?
-- TODO how often to actuall flush?
hWriteTree :: SearchConfig -> LogCfg -> Handle -> HashTree a -> IO ()
hWriteTree cfg lCfg h tree = do
  hWriteHeader   h $ hashExcludeRegexes cfg
  hWriteTreeBody lCfg h tree
  hWriteFooter   h

-- TODO how often to actually flush?
hWriteTreeBody :: LogCfg -> Handle -> HashTree a -> IO ()
hWriteTreeBody lCfg h tree = mapM_ (\l -> B8.hPutStrLn h l >> hFlush h) (serializeTree lCfg tree)

-- This is the only official way to construct a `HashLine`, because they don't
-- make sense in isolation; each `Dir` needs to be preceded in the list by its
-- contents to reconstruct the tree structure.
flattenTree :: LogCfg -> HashTree a -> [HashLine]
flattenTree lCfg = flattenTree' lCfg (Depth 0)

-- TODO need to handle unicode here?
-- TODO does this affect memory usage?
flattenTree' :: LogCfg -> Depth -> HashTree a -> [HashLine]
flattenTree' lCfg (Depth d) _ | d < 0 = die (addLogContext lCfg "flattenTree'") "called with negative depth"
flattenTree' _ d (Err {errName=n, errMsg=m}) = [ErrLine (d, m, n)]
flattenTree' _ d (File {nodeData=nd})
  = [HashLine (F, d, hash nd, modTime nd, nBytes nd, 1, name nd, Nothing)]
flattenTree' _ d (Link {linkData=ld, nodeData=nd, linkTarget=lt}) =
  let tt = if isNothing ld then B else L
  in [HashLine (tt, d, hash nd, modTime nd, nBytes nd, 1, name nd, Just lt)]
flattenTree' lCfg (Depth d) (Dir  {nodeData=nd, dirContents=cs, nNodes=f})
  = subtrees ++ [wholeDir]
  where
    n = name nd
    subtrees = concatMap (flattenTree' lCfg $ Depth $ d+1) (reverse $ sortContentsByName cs) -- TODO reverse?
    wholeDir = HashLine (D, Depth d, hash nd, modTime nd, nBytes nd, f, n, Nothing)

-- this is to catch the case where it tries to write the same file twice
-- (happened once because of macos filename case-insensitivity)
assertDoesNotExist :: LogCfg -> OsPath -> IO ()
assertDoesNotExist lCfg path = do
  exists <- SDO.doesPathExist path
  when exists $ do
    path' <- decodeFS path
    die (addLogContext lCfg "assertDoesNotExist") $ B8.pack $ "duplicate write to " ++ show path'

assertExists :: LogCfg -> OsPath -> IO ()
assertExists lCfg path = do
  exists <- SDO.doesPathExist path
  unless exists $ do
    path' <- decodeFS path
    die (addLogContext lCfg "assertExists") $ B8.pack $ "failed to write " ++ show path'

{- Take a generated `TestTree` and write it to a tree of tmpfiles.
 - Note that unlike the write-tree-to-lines functions above, this goes in forward order.
 - (It has to, because root directories must be created before their children)
 - TODO should this be NoLog?
 -}
writeTestTreeDir :: LogCfg -> OsPath -> TestTree -> IO ()
writeTestTreeDir lCfg path tree = do
  let parent = takeDirectory path
      tree'  = renameRoot (Name $ takeBaseName path) tree
  writeTestTreeDir' lCfg parent tree'
  assertExists lCfg path

writeTestTreeDir' :: LogCfg -> OsPath -> TestTree -> IO ()
writeTestTreeDir' lCfg parent (Err {}) = return () -- TODO print a warning? write to the file?

writeTestTreeDir' lCfg parent l@(Link {nodeData=nd}) = do
  let path = parent </> unName (name nd)
  assertDoesNotExist lCfg path
  -- Target comes first, then the file we're writing (like `ln -s`)
  SDO.createFileLink (linkTarget l) path
  assertExists lCfg path

writeTestTreeDir' lCfg parent (File {nodeData=nd, fileData = bs}) = do
  SDO.createDirectoryIfMissing True parent
  let path = parent </> unName (name nd)
  assertDoesNotExist lCfg path
  SFO.writeFile' path bs
  assertExists lCfg path

writeTestTreeDir' lCfg parent (Dir {nodeData=nd, dirContents = cs}) = do
  let root = parent </> unName (name nd)
  assertDoesNotExist lCfg root
  SDO.createDirectoryIfMissing True root
  assertExists lCfg root
  mapM_ (writeTestTreeDir' lCfg root) (sortContentsByName cs) -- TODO remove sort?
