module System.Directory.BigTrees.HashTree.Write where

-- import Control.DeepSeq (force)
import Control.Monad (when)
import qualified Data.ByteString.Char8 as B8
import qualified System.Directory as SD
import System.Directory.BigTrees.HashLine (HashLine (..), Depth (Depth), NNodes(..),
                                           TreeType (D, F), prettyLine)
import System.Directory.BigTrees.HashTree.Base (HashTree(..), NodeData(..), TestTree)
import System.Directory.BigTrees.Name (n2fp)
import System.FilePath (splitPath, (</>))
import System.IO (IOMode (..), hFlush, stdout, withFile, Handle)

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
writeTree :: FilePath -> HashTree a -> IO ()
writeTree path tree = withFile path WriteMode $ \h -> hWriteTree h tree

hWriteTree :: Handle -> HashTree a -> IO ()
hWriteTree h tree = mapM_ (B8.hPutStrLn h) (serializeTree tree)

-- This is the only official way to construct a `HashLine`, because they don't
-- make sense in isolation; each `Dir` needs to be preceded in the list by its
-- dirContents to reconstruct the tree structure.
flattenTree :: HashTree a -> [HashLine]
flattenTree = flattenTree' ""

-- TODO need to handle unicode here?
-- TODO does this affect memory usage?
flattenTree' :: FilePath -> HashTree a -> [HashLine]
flattenTree' dir (File {nodeData=nd})
  = [HashLine (F, Depth $ length (splitPath dir), hash nd, modTime nd, nBytes nd, 1, name nd)]
flattenTree' dir (Dir  {nodeData=nd, dirContents=cs, nNodes=f})
  = subtrees ++ [wholeDir]
  where
    n = name nd
    subtrees = concatMap (flattenTree' $ dir </> n2fp n) cs -- TODO nappend?
    wholeDir = HashLine (D, Depth $ length (splitPath dir), hash nd, modTime nd, nBytes nd, f, n)

-- this is to catch the case where it tries to write the same file twice
-- (happened once because of macos filename case-insensitivity)
assertNoFile :: FilePath -> IO ()
assertNoFile path = do
  exists <- SD.doesPathExist path
  when exists $ error $ "duplicate write to: '" ++ path ++ "'"

{- Take a generated `TestTree` and write it to a tree of tmpfiles.
 - Note that this calls itself recursively.
 - Note also that when you call this at the top level,
 - `root` should refer to the parent dir of your tree!
 - (Yes this is confusing, and should be changed if it will be user facing)
 -
 - TODO take an anchored tree rather than this separate root,
 -      because it's ambiguous what to do with the root name otherwise
 -}
writeTestTreeDir :: FilePath -> TestTree -> IO ()
writeTestTreeDir root (File {nodeData=nd, fileData = bs}) = do
  -- SD.createDirectoryIfMissing True root -- TODO remove
  let path = root </> n2fp (name nd) -- TODO use IsName here!
  -- assertNoFile path
  B8.writeFile path bs
writeTestTreeDir root (Dir {nodeData=nd, dirContents = cs}) = do
  let root' = root </> n2fp (name nd) -- TODO use IsName here!
  -- assertNoFile root'
  -- putStrLn $ "write test dir: " ++ root'
  SD.createDirectoryIfMissing False root' -- TODO true?
  mapM_ (writeTestTreeDir root') cs
