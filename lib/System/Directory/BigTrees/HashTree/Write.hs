module System.Directory.BigTrees.HashTree.Write where

import Control.Monad (when)
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (isNothing)
import qualified System.Directory.OsPath as SD
import System.Directory.BigTrees.HashLine (Depth (Depth), HashLine (..), NNodes (..), TreeType (..),
                                           prettyLine)
import System.Directory.BigTrees.HashTree.Base (HashTree (..), NodeData (..), TestTree)
import System.Directory.BigTrees.HeadFoot (hWriteFooter, hWriteHeader)
import System.Directory.BigTrees.Name (unName)
import System.OsPath (splitPath, (</>), OsPath, decodeFS)
import System.IO (Handle, IOMode (..), hFlush, stdout)
import qualified System.File.OsPath as SFO

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
hWriteTree :: [String] -> Handle -> HashTree a -> IO ()
hWriteTree es h tree = do
  hWriteHeader   h es
  hWriteTreeBody h tree
  hWriteFooter   h

hWriteTreeBody :: Handle -> HashTree a -> IO ()
hWriteTreeBody h tree = mapM_ (B8.hPutStrLn h) (serializeTree tree)

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
  = [HashLine (F, d, hash nd, modTime nd, nBytes nd, 1, name nd)]
flattenTree' d (Link {linkData=ld, nodeData=nd}) =
  let tt = if isNothing ld then B else L
  in [HashLine (tt, d, hash nd, modTime nd, nBytes nd, 1, name nd)]
flattenTree' (Depth d) (Dir  {nodeData=nd, dirContents=cs, nNodes=f})
  = subtrees ++ [wholeDir]
  where
    n = name nd
    subtrees = concatMap (flattenTree' $ Depth $ d+1) cs
    wholeDir = HashLine (D, Depth d, hash nd, modTime nd, nBytes nd, f, n)

-- this is to catch the case where it tries to write the same file twice
-- (happened once because of macos filename case-insensitivity)
assertNoFile :: OsPath -> IO ()
assertNoFile path = do
  exists <- SD.doesPathExist path
  when exists $ do
    path' <- decodeFS path
    error $ "duplicate write to: '" ++ path' ++ "'"

{- Take a generated `TestTree` and write it to a tree of tmpfiles.
 - Note that this calls itself recursively.
 - Note also that when you call this at the top level,
 - `root` should refer to the parent dir of your tree!
 - (Yes this is confusing, and should be changed if it will be user facing)
 -
 - TODO take an anchored tree rather than this separate root,
 -      because it's ambiguous what to do with the root name otherwise
 -}
writeTestTreeDir :: OsPath -> TestTree -> IO ()
writeTestTreeDir root (File {nodeData=nd, fileData = bs}) = do
  -- SD.createDirectoryIfMissing True root -- TODO remove
  let path = root </> (unName $ name nd)
  assertNoFile path
  SFO.writeFile' path bs
writeTestTreeDir root (Dir {nodeData=nd, dirContents = cs}) = do
  let root' = root </> (unName $ name nd)
  assertNoFile root'
  -- putStrLn $ "write test dir: " ++ root'
  SD.createDirectoryIfMissing False root' -- TODO true?
  mapM_ (writeTestTreeDir root') cs
-- TODO finish Link branch here!
