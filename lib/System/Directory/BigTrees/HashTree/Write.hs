{-# LANGUAGE OverloadedStrings #-}

module System.Directory.BigTrees.HashTree.Write where

import Control.Monad (unless, when)
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (isNothing)
import System.Directory.BigTrees.HashLine (Depth (Depth), HashLine (..), NNodes (..), TreeType (..),
                                           prettyLine)
import System.Directory.BigTrees.HashTree.Base (HashTree (..), NodeData (..), TestTree)
import System.Directory.BigTrees.HashTree.Search (SearchConfig (..))
import System.Directory.BigTrees.HeadFoot (hWriteFooter, hWriteHeader)
import System.Directory.BigTrees.Name (unName)
import System.Directory.BigTrees.Logging (LogCfg, die, addLogContext)
import qualified System.Directory.OsPath as SDO
import qualified System.File.OsPath as SFO
import System.IO (Handle, IOMode (..), hFlush, stdout)
import System.OsPath (OsPath, decodeFS, splitPath, (</>))

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
-- dirContents to reconstruct the tree structure.
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
    subtrees = concatMap (flattenTree' lCfg $ Depth $ d+1) cs
    wholeDir = HashLine (D, Depth d, hash nd, modTime nd, nBytes nd, f, n, Nothing)

-- this is to catch the case where it tries to write the same file twice
-- (happened once because of macos filename case-insensitivity)
assertNoFile :: LogCfg -> OsPath -> IO ()
assertNoFile lCfg path = do
  exists <- SDO.doesPathExist path
  when exists $ do
    path' <- decodeFS path
    -- putStrLn $ "duplicate write: " ++ show path'
    die (addLogContext lCfg "assertNoFile") $ B8.pack $ "duplicate write: " ++ show path'

assertFile :: LogCfg -> OsPath -> IO ()
assertFile lCfg path = do
  exists <- SDO.doesPathExist path
  unless exists $ do
    path' <- decodeFS path
    -- putStrLn $ "failed to write: " ++ show path'
    die (addLogContext lCfg "assertFile") $ B8.pack $ "failed to write: " ++ show path'

{- Take a generated `TestTree` and write it to a tree of tmpfiles.
 - Note that this calls itself recursively.
 - Note also that when you call this at the top level,
 - `root` should refer to the parent dir of your tree!
 - (Yes this is confusing, and should be changed if it will be user facing)
 - TODO should this be NoLog?
 -}
writeTestTreeDir :: LogCfg -> OsPath -> TestTree -> IO ()

writeTestTreeDir lCfg root (Err {}) = return () -- TODO print a warning?

writeTestTreeDir lCfg root l@(Link {nodeData=nd}) = do
  let path = root </> unName (name nd)
  assertNoFile lCfg path
  -- Target comes first, then the file we're writing (like `ln -s`)
  SDO.createFileLink (linkTarget l) path
  assertFile lCfg path

writeTestTreeDir lCfg root (File {nodeData=nd, fileData = bs}) = do
  -- SDO.createDirectoryIfMissing True root -- TODO remove
  let path = root </> unName (name nd)
  assertNoFile lCfg path
  SFO.writeFile' path bs
  assertFile lCfg path

writeTestTreeDir lCfg root (Dir {nodeData=nd, dirContents = cs}) = do
  let root' = root </> unName (name nd)
  assertNoFile lCfg root'
  -- putStrLn $ "write test dir: " ++ show root'
  SDO.createDirectoryIfMissing True root' -- TODO true?
  assertFile lCfg root'
  mapM_ (writeTestTreeDir lCfg root') cs
