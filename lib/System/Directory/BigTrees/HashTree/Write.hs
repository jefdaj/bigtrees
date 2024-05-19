module System.Directory.BigTrees.HashTree.Write where

-- import Control.DeepSeq (force)
import Control.Monad (when)
import qualified Data.ByteString.Char8 as B8
import Data.Store (encode)
import qualified System.Directory as SD
import System.Directory.BigTrees.Name (n2fp)
import System.Directory.BigTrees.HashLine (HashLine (..), IndentLevel (IndentLevel),
                                           TreeType (D, F), prettyHashLine)
import System.Directory.BigTrees.HashTree.Base (HashTree (Dir, File, contents, fileData, name),
                                                ProdTree, TestTree)
import System.FilePath (splitPath, (</>))
import System.IO (IOMode (..), hFlush, stdout, withFile)


-- TODO can Foldable or Traversable simplify these?
-- TODO need to handle unicode here?
-- TODO does map evaluation influence memory usage?
-- TODO create a single ByteString rather than a list for compression?
serializeTree :: ProdTree -> [B8.ByteString]
serializeTree = map prettyHashLine . flattenTree

-- TODO remove and make this a special case of WriteTree? or vice versa?
printTree :: ProdTree -> IO ()
printTree = mapM_ printLine . flattenTree
  where
    -- TODO don't flush every line
    printLine l = putStrLn (B8.unpack $ prettyHashLine l) >> hFlush stdout

-- this uses a handle for streaming output, which turns out to be important for memory usage
-- TODO rename writeHashes? this is a confusing way to say that
writeTree :: FilePath -> ProdTree -> IO ()
writeTree path tree = withFile path WriteMode $ \h ->
  mapM_ (B8.hPutStrLn h) (serializeTree tree)

writeBinTree :: FilePath -> ProdTree -> IO ()
writeBinTree path tree = B8.writeFile path $ encode tree

flattenTree :: ProdTree -> [HashLine]
flattenTree = flattenTree' ""

-- TODO need to handle unicode here?
-- TODO does this affect memory usage?
flattenTree' :: FilePath -> ProdTree -> [HashLine]
flattenTree' dir (File n h ()  ) = [HashLine (F, IndentLevel $ length (splitPath dir), h, n)]
flattenTree' dir (Dir  n h cs _) = subtrees ++ [wholeDir]
  where
    subtrees = concatMap (flattenTree' $ dir </> n2fp n) cs
    wholeDir = HashLine (D, IndentLevel $ length (splitPath dir), h, n)

-- this is to catch the case where it tries to write the same file twice
-- (happened once because of macos filename case-insensitivity)
assertNoFile :: FilePath -> IO ()
assertNoFile path = do
  exists <- SD.doesPathExist path
  when exists $ error $ "duplicate write to: '" ++ path ++ "'"

{- Take a generated `TestTree` and write it to a tree of tmpfiles.
 - Note that this calls itself recursively.
 -}
writeTestTreeDir :: FilePath -> TestTree -> IO ()
writeTestTreeDir root (File {name = n, fileData = bs}) = do
  let path = root </> n2fp n
  assertNoFile path
  B8.writeFile path bs
writeTestTreeDir root (Dir {name = n, contents = cs}) = do
  let root' = root </> n2fp n
  assertNoFile root'
  -- putStrLn $ "write test dir: " ++ root'
  SD.createDirectoryIfMissing True root' -- TODO false here
  mapM_ (writeTestTreeDir root') cs

