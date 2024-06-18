module System.Directory.BigTrees.HashTree.Write where

import Control.Monad (when, unless)
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (isNothing)
import System.Directory.BigTrees.HashLine (Depth (Depth), HashLine (..), NNodes (..), TreeType (..),
                                           prettyLine)
import System.Directory.BigTrees.HashTree.Base (HashTree (..), NodeData (..), TestTree)
import System.Directory.BigTrees.HeadFoot (hWriteFooter, hWriteHeader)
import System.Directory.BigTrees.Name (unName)
import qualified System.Directory.OsPath as SDO
import qualified System.File.OsPath as SFO
import System.IO (Handle, IOMode (..), hFlush, stdout)
import System.OsPath (OsPath, decodeFS, splitPath, (</>))

-- import Debug.Trace

-- TODO can Foldable or Traversable simplify these?
-- TODO need to handle unicode here?
-- TODO does map evaluation influence memory usage?
-- TODO create a single ByteString rather than a list for compression?
serializeTree :: HashTree a -> [B8.ByteString]
serializeTree = map (prettyLine Nothing) . flattenTree

-- TODO remove and make this a special case of WriteTree? or vice versa?
printTree :: HashTree a -> IO ()
printTree = mapM_ printLine . flattenTree
  where
    -- TODO don't flush every line
    printLine l = putStrLn (B8.unpack $ prettyLine Nothing l) >> hFlush stdout

-- this uses a handle for streaming output, which turns out to be important for memory usage
-- TODO rename writeHashes? this is a confusing way to say that
-- TODO how much of the config should live in the library vs the app, if we're writing it?
writeTree :: [String] -> OsPath -> HashTree a -> IO ()
writeTree es path tree = SFO.withFile path WriteMode $ \h -> hWriteTree es h tree

-- TODO excludes type alias?
-- TODO how often to actuall flush?
hWriteTree :: [String] -> Handle -> HashTree a -> IO ()
hWriteTree es h tree = do
  hWriteHeader   h es
  hWriteTreeBody h tree
  hWriteFooter   h

-- TODO how often to actually flush?
hWriteTreeBody :: Handle -> HashTree a -> IO ()
hWriteTreeBody h tree = mapM_ (\l -> B8.hPutStrLn h l >> hFlush h) (serializeTree tree)

-- This is the only official way to construct a `HashLine`, because they don't
-- make sense in isolation; each `Dir` needs to be preceded in the list by its
-- dirContents to reconstruct the tree structure.
flattenTree :: HashTree a -> [HashLine]
flattenTree = flattenTree' (Depth 0)

-- TODO need to handle unicode here?
-- TODO does this affect memory usage?
flattenTree' :: Depth -> HashTree a -> [HashLine]
flattenTree' (Depth d) _ | d < 0 = error "tried to call flattenTree' with negative depth"
flattenTree' d (Err {errName=n, errMsg=m}) = [ErrLine (d, m, n)]
flattenTree' d (File {nodeData=nd})
  = [HashLine (F, d, hash nd, modTime nd, nBytes nd, 1, name nd, Nothing)]
flattenTree' d (Link {linkData=ld, nodeData=nd, linkTarget=lt}) =
  let tt = if isNothing ld then B else L
  in [HashLine (tt, d, hash nd, modTime nd, nBytes nd, 1, name nd, Just lt)]
flattenTree' (Depth d) (Dir  {nodeData=nd, dirContents=cs, nNodes=f})
  = subtrees ++ [wholeDir]
  where
    n = name nd
    subtrees = concatMap (flattenTree' $ Depth $ d+1) cs
    wholeDir = HashLine (D, Depth d, hash nd, modTime nd, nBytes nd, f, n, Nothing)

-- this is to catch the case where it tries to write the same file twice
-- (happened once because of macos filename case-insensitivity)
assertNoFile :: OsPath -> IO ()
assertNoFile path = do
  exists <- SDO.doesPathExist path
  when exists $ do
    path' <- decodeFS path
    -- putStrLn $ "duplicate write: " ++ show path'
    error $ "duplicate write: " ++ show path'

assertFile :: OsPath -> IO ()
assertFile path = do
  exists <- SDO.doesPathExist path
  unless exists $ do
    path' <- decodeFS path
    -- putStrLn $ "failed to write: " ++ show path'
    error $ "failed to write: " ++ show path'

{- Take a generated `TestTree` and write it to a tree of tmpfiles.
 - Note that this calls itself recursively.
 - Note also that when you call this at the top level,
 - `root` should refer to the parent dir of your tree!
 - (Yes this is confusing, and should be changed if it will be user facing)
 -}
writeTestTreeDir :: OsPath -> TestTree -> IO ()

writeTestTreeDir root (Err {}) = return () -- TODO print a warning?

writeTestTreeDir root l@(Link {nodeData=nd}) = do
  let path = root </> unName (name nd)
  assertNoFile path
  -- Target comes first, then the file we're writing (like `ln -s`)
  SDO.createFileLink (linkTarget l) path
  assertFile path

writeTestTreeDir root (File {nodeData=nd, fileData = bs}) = do
  -- SDO.createDirectoryIfMissing True root -- TODO remove
  let path = root </> unName (name nd)
  assertNoFile path
  SFO.writeFile' path bs
  assertFile path

writeTestTreeDir root (Dir {nodeData=nd, dirContents = cs}) = do
  let root' = root </> unName (name nd)
  assertNoFile root'
  -- putStrLn $ "write test dir: " ++ show root'
  SDO.createDirectoryIfMissing True root' -- TODO true?
  assertFile root'
  mapM_ (writeTestTreeDir root') cs
