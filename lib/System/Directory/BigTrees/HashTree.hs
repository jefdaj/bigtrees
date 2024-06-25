{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Use camelCase" #-}

module System.Directory.BigTrees.HashTree

  ( HashTree(..)
  , NodeData(..)
  , ProdTree
  , TestTree

  , accTrees -- TODO hide this better?
  , addSubTree
  , buildProdTree
  , buildTree
  , buildTreeWithGrafts
  , dropTo
  , printTree
  , readOrBuildTree
  , readTree
  , hReadTree
  , rmSubTree
  , treeContainsHash
  , treeContainsPath
  , SearchConfig(..)
  , writeTree
  , hWriteTree
  , listTreePaths
  , treeName
  , treeHash
  , treeNBytes
  , treeModTime
  , treeType
  , readLastHashLineAndFooter
  , sumNodes
  , treeEqIgnoringModTime

  -- for testing
  , roundtripTestTreeToTmpdir
  , dropFileData
  , writeTestTreeDir
  , isErr
  , prop_roundtrip_ProdTree_to_ByteString
  , prop_roundtrip_ProdTree_to_bigtree_file
  , prop_roundtrip_TestTree_to_tmpdir
  , unit_tree_from_bad_path_is_Err
  , unit_roundtrip_Err_to_bigtree_file
  , unit_buildProdTree_catches_permission_error
  , bench_roundtrip_ProdTree_to_bigtree_file

  )
  where

-- TODO would be better to adapt AnchoredDirTree with a custom node type than re-implement stuff


import qualified Data.ByteString.Char8 as B8
import System.Directory.BigTrees.HashLine (ErrMsg (..))
import System.Directory.BigTrees.Name (Name (..))
import qualified System.Directory.OsPath as SDO
import System.OsPath (OsPath, encodeFS, osp, (</>))
-- import System.FilePath.Glob (Pattern)
import Control.Monad (unless)
import qualified System.FilePath as SF
import System.IO (IOMode (..), hClose)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import Test.QuickCheck (Arbitrary (..), Property, arbitrary, generate, resize)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)

import qualified Control.Concurrent.Thread.Delay as D
import qualified Data.Knob as K
import Data.List (isInfixOf)
import System.Directory.BigTrees.HashTree.Base (HashTree (..), NodeData (..), ProdTree, TestTree,
                                                dropFileData, isErr, renameRoot, sumNodes,
                                                treeEqIgnoringModTime, treeHash, treeModTime,
                                                treeNBytes, treeName, treeType)
import System.Directory.BigTrees.HashTree.Build (buildProdTree, buildTree, buildTreeWithGrafts, readTestTree)
import System.Directory.BigTrees.HashTree.Edit (addSubTree, rmSubTree)
import System.Directory.BigTrees.HashTree.Find (listTreePaths)
import System.Directory.BigTrees.HashTree.Read (accTrees, hReadTree, readLastHashLineAndFooter,
                                                readTree)
import System.Directory.BigTrees.HashTree.Search (SearchConfig (..), dropTo, emptySearchConfig,
                                                  treeContainsHash, treeContainsPath)
import System.Directory.BigTrees.HashTree.Write (hWriteTree, printTree, serializeTree,
                                                 writeTestTreeDir, writeTree)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (cwd, proc, readCreateProcess)
import qualified Test.HUnit as HU

-- import System.Directory.BigTrees.Util (absolutePath)

-- import qualified Data.ByteString.Char8 as B
-- import Text.Pretty.Simple (pPrint)


-- If passed a file this assumes it contains hashes and builds a tree of them;
-- If passed a dir it will scan it first and then build the tree.
-- TODO don't assume??
readOrBuildTree :: SearchConfig -> Bool -> OsPath -> IO ProdTree
readOrBuildTree cfg verbose path = do
  isDir  <- SDO.doesDirectoryExist path
  isFile <- SDO.doesFileExist      path
  if      isFile then readTree cfg path
  else if isDir then buildProdTree cfg verbose path
  else error $ "No such file: " ++ show path

-- TODO test tree in haskell
-- TODO test dir
-- TODO test annex

-- TODO unit_build_tree_from_dir
-- TODO read_tree
-- TODO serialize_tree
-- TODO write_tree
-- TODO print_tree
-- TODO flatten_tree

-- prop_roundtrip_ProdTree_to_bigtree_file ::

--     describe "HashTree" $ do
--       describe "HashTree" $ do
--         it "builds a tree from the test annex" $ pendingWith "need annex test harness"

-- TODO prop_confirm_dir_hashes too?

-- TODO what's right here but wrong in the roundtrip to bytestring ones?
prop_roundtrip_ProdTree_to_ByteString :: Property
prop_roundtrip_ProdTree_to_ByteString = monadicIO $ do
  knob <- K.newKnob mempty
  (t1 :: ProdTree) <- pick arbitrary
  let cfg = emptySearchConfig
  K.withFileHandle knob "knob" WriteMode $ \h -> hWriteTree cfg h t1 -- TODO hClose?
  t2 <- run $ K.withFileHandle knob "knob" ReadMode $ hReadTree cfg 4096
  assert $ t2 == t1

bench_roundtrip_ProdTree_to_bigtree_file :: Int -> IO ()
bench_roundtrip_ProdTree_to_bigtree_file n = do
  (t1 :: ProdTree) <- generate $ resize n arbitrary
  t2 <- roundtripProdTreeToBigtreeFile t1
  -- assert $ t2 == t1
  return ()

-- TODO unify with the knob version above
roundtripProdTreeToBigtreeFile :: ProdTree -> IO ProdTree
roundtripProdTreeToBigtreeFile t =
  withSystemTempFile "bigtrees" $ \path hdl -> do
    path' <- encodeFS path
    hClose hdl
    let cfg = emptySearchConfig
    writeTree cfg path' t -- TODO exclude defaultConfig?
    readTree cfg path'

prop_roundtrip_ProdTree_to_bigtree_file :: Property
prop_roundtrip_ProdTree_to_bigtree_file = monadicIO $ do
  t1 <- pick arbitrary
  t2 <- run $ roundtripProdTreeToBigtreeFile t1
  assert $ t2 == t1

-- the tests above round-trip to single files describing trees, whereas this
-- one round-trips to an actual directory tree on disk
-- note that you have to drop the bytestrings from the original testtree to compare them
-- TODO oh, have to test equality ignoring mod times, right? otherwise they'll always update
roundtripTestTreeToTmpdir :: TestTree -> IO TestTree
roundtripTestTreeToTmpdir t =

  withSystemTempDirectory "bigtrees" $ \tmpDir -> do
    tmpDir' <- encodeFS tmpDir
    D.delay 100000
    -- let tmpRoot = tmpDir </> "round-trip-tests" -- TODO use root
    -- SD.createDirectoryIfMissing True tmpDir -- TODO False?
    -- SD.removePathForcibly tmpDir -- TODO remove

    -- This is a little confusing, but the FilePath here should be the *parent*
    -- within which to write the root tree dir...
    writeTestTreeDir tmpDir' t
    D.delay 100000

    -- ... but then when reading it back in we need the full path including the
    -- root tree dir name.
    let treeRootDir = tmpDir' </> unName (treeName t)
    readTestTree emptySearchConfig False treeRootDir
    -- parent <- readTestTree Nothing False [] tmpDir'
    -- return $ head $ dirContents parent

-- TODO is the forcing unnecessary?
prop_roundtrip_TestTree_to_tmpdir :: Property
prop_roundtrip_TestTree_to_tmpdir = monadicIO $ do
  t1 <- pick arbitrary
  run $ D.delay 100000
  t2 <- run $ roundtripTestTreeToTmpdir t1
  run $ D.delay 100000
  unless (treeEqIgnoringModTime t1 t2) $ do
    run $ print t1
    run $ print t2
  assert $ treeEqIgnoringModTime t1 t2

unit_tree_from_bad_path_is_Err :: HU.Assertion
unit_tree_from_bad_path_is_Err =
  withSystemTempDirectory "bigtrees" $ \tmpDir -> do
    tmpDir' <- encodeFS tmpDir
    let badPath = tmpDir' </> [osp|doesnotexist|]
    tree <- buildProdTree emptySearchConfig False badPath
    HU.assertBool "tree built from non-existent path should be Err" $ isErr tree

unit_roundtrip_Err_to_bigtree_file :: HU.Assertion
unit_roundtrip_Err_to_bigtree_file = do
  withSystemTempDirectory "bigtrees" $ \tmpDir -> do
    tmpDir' <- encodeFS tmpDir
    let badPath = tmpDir' </> [osp|doesnotexist|]
    t1 <- buildProdTree emptySearchConfig False badPath
    t2 <- roundtripProdTreeToBigtreeFile t1
    -- TODO is there a good way to communicate the name to the parser?
    let t2' = renameRoot (Name [osp|doesnotexist|]) t2
    HU.assert $ t2' == t1

-- TODO rename to be more general? i imagine it should apply to any IO error
unit_buildProdTree_catches_permission_error :: HU.Assertion
unit_buildProdTree_catches_permission_error = do
  withSystemTempDirectory "bigtrees" $ \tmpDir -> do
    let badPath = tmpDir SF.</> badName
    badPath' <- encodeFS badPath
    _ <- readCreateProcess ((proc "touch" [badPath]      ) {cwd = Just tmpDir}) ""
    _ <- readCreateProcess ((proc "chmod" ["-r", badPath]) {cwd = Just tmpDir}) ""
    t1 <- buildProdTree emptySearchConfig False badPath'
    HU.assertBool "Err looks right" $ errLooksRight t1
  where
    badName = "file-without-read-permission.txt"
    -- TODO proper idiom for this kind of test
    errLooksRight e@(Err { errName = n, errMsg = ErrMsg m})
      = "permission denied" `isInfixOf` m -- TODO add back check for name == badName?
    errLooksRight _ = False
