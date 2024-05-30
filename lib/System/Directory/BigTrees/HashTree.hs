{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
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
  , dropTo
  , printTree
  , readOrBuildTree
  , readTree
  , rmSubTree
  , treeContainsHash
  , treeContainsPath
  , writeTree
  , hWriteTree
  , printTreePaths
  , treeName
  , treeNBytes
  , treeModTime
  , treeType

  -- for testing
  , sumNodes
  , roundtripTestTreeToDir
  , dropFileData
  , writeTestTreeDir
  , isErr
  , prop_roundtrip_ProdTree_to_ByteString
  , prop_roundtrip_ProdTree_to_hashes
  , prop_roundtrip_TestTree_to_dir
  , unit_tree_from_bad_path_is_Err
  , unit_roundtrip_Err_to_hashes
  , unit_buildProdTree_catches_permission_error

  )
  where

-- TODO would be better to adapt AnchoredDirTree with a custom node type than re-implement stuff


import qualified Data.ByteString.Char8 as B8
import qualified System.Directory as SD
import System.Directory.BigTrees.Name (n2fp, Name(..))
import System.Directory.BigTrees.HashLine (ErrMsg(..))
import System.FilePath ((</>))
-- import System.FilePath.Glob (Pattern)
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import Test.QuickCheck (Arbitrary (..), Property, arbitrary)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)

import System.Directory.BigTrees.HashTree.Base (HashTree (..), NodeData(..), ProdTree, TestTree, sumNodes,
                                                dropFileData, isErr, renameRoot, treeName, treeModTime, treeNBytes, treeType)
import System.Directory.BigTrees.HashTree.Build (buildProdTree, buildTree)
import System.Directory.BigTrees.HashTree.Edit (addSubTree, rmSubTree)
import System.Directory.BigTrees.HashTree.Find (Filter (..), pathMatches, printTreePaths)
import System.Directory.BigTrees.HashTree.Read (accTrees, deserializeTree, readTestTree, readTree)
import System.Directory.BigTrees.HashTree.Search (dropTo, treeContainsHash, treeContainsPath)
import System.Directory.BigTrees.HashTree.Write (printTree, serializeTree, writeTestTreeDir,
                                                 hWriteTree, writeTree)
import qualified Test.HUnit as HU
import System.Process (cwd, proc, readCreateProcess)
import System.IO.Temp (withSystemTempDirectory)
import Data.List (isInfixOf)

-- import System.Directory.BigTrees.Util (absolutePath)

-- import qualified Data.ByteString.Char8 as B
-- import Text.Pretty.Simple (pPrint)


-- If passed a file this assumes it contains hashes and builds a tree of them;
-- If passed a dir it will scan it first and then build the tree.
-- TODO don't assume??
readOrBuildTree :: Bool -> Maybe Int -> [String] -> FilePath -> IO ProdTree
readOrBuildTree verbose mmaxdepth excludes path = do
  isDir  <- SD.doesDirectoryExist path
  isFile <- SD.doesFileExist      path
  if      isFile then readTree mmaxdepth path
  else if isDir then buildProdTree verbose excludes path
  else error $ "No such file: '" ++ path ++ "'"

-- TODO test tree in haskell
-- TODO test dir
-- TODO test annex

-- TODO unit_build_tree_from_dir
-- TODO read_tree
-- TODO serialize_tree
-- TODO write_tree
-- TODO print_tree
-- TODO flatten_tree

-- prop_roundtrip_ProdTree_to_hashes ::

--     describe "HashTree" $ do
--       describe "HashTree" $ do
--         it "builds a tree from the test annex" $ pendingWith "need annex test harness"

-- TODO prop_confirm_dir_hashes too?

-- TODO what's right here but wrong in the roundtrip to bytestring ones?
prop_roundtrip_ProdTree_to_ByteString :: ProdTree -> Bool
prop_roundtrip_ProdTree_to_ByteString t = t' == t
  where
    bs = B8.unlines $ serializeTree t -- TODO why didn't it include the unlines part again?
    t' = deserializeTree Nothing bs

-- TODO put in the main tmpdir
roundtripProdTreeToHashes :: ProdTree -> IO ProdTree
roundtripProdTreeToHashes t =
  withSystemTempFile "bigtrees" $ \path hdl -> do
    hClose hdl
    writeTree path t
    readTree Nothing path

prop_roundtrip_ProdTree_to_hashes :: Property
prop_roundtrip_ProdTree_to_hashes = monadicIO $ do
  t1 <- pick arbitrary
  t2 <- run $ roundtripProdTreeToHashes t1
  assert $ t2 == t1

-- the tests above round-trip to single files describing trees, whereas this
-- one round-trips to an actual directory tree on disk
-- note that you have to drop the bytestrings from the original testtree to compare them
-- TODO oh, have to test equality ignoring mod times, right? otherwise they'll always update
roundtripTestTreeToDir :: TestTree -> IO TestTree
roundtripTestTreeToDir t =

  withSystemTempDirectory "bigtrees" $ \tmpDir -> do
    -- let tmpRoot = tmpDir </> "round-trip-tests" -- TODO use root
    -- SD.createDirectoryIfMissing True tmpDir -- TODO False?
    -- SD.removePathForcibly tmpDir -- TODO remove

    -- This is a little confusing, but the FilePath here should be the *parent*
    -- within which to write the root tree dir...
    writeTestTreeDir tmpDir t

    -- ... but then when reading it back in we need the full path including the
    -- root tree dir name.
    let treeRootDir = tmpDir </> n2fp (treeName t) -- TODO use IsName here
    readTestTree Nothing False [] treeRootDir

-- TODO is the forcing unnecessary?
prop_roundtrip_TestTree_to_dir :: Property
prop_roundtrip_TestTree_to_dir = monadicIO $ do
  t1 <- pick arbitrary
  t2 <- run $ roundtripTestTreeToDir t1
  assert $ t2 == t1

unit_tree_from_bad_path_is_Err :: HU.Assertion
unit_tree_from_bad_path_is_Err =
  withSystemTempDirectory "bigtrees" $ \tmpDir -> do
    let badPath = tmpDir </> "doesnotexist"
    tree <- buildProdTree False [] badPath
    HU.assertBool "tree built from non-existent path should be Err" $ isErr tree

unit_roundtrip_Err_to_hashes :: HU.Assertion
unit_roundtrip_Err_to_hashes = do
  withSystemTempDirectory "bigtrees" $ \tmpDir -> do
    let badPath = tmpDir </> "doesnotexist"
    t1 <- buildProdTree False [] badPath
    t2 <- roundtripProdTreeToHashes t1
    -- TODO is there a good way to communicate the name to the parser?
    let t2' = renameRoot "doesnotexist" t2
    HU.assert $ t2' == t1

-- TODO rename to be more general? i imagine it should apply to any IO error
unit_buildProdTree_catches_permission_error :: HU.Assertion
unit_buildProdTree_catches_permission_error = do
  withSystemTempDirectory "bigtrees" $ \tmpDir -> do
    let badPath = tmpDir </> badName
    _ <- readCreateProcess ((proc "touch" [badPath]      ) {cwd = Just tmpDir}) ""
    _ <- readCreateProcess ((proc "chmod" ["-r", badPath]) {cwd = Just tmpDir}) ""
    t1 <- buildProdTree False [] badPath
    HU.assertBool "Err looks right" $ errLooksRight t1
  where
    badName = "file-without-read-permission.txt"
    -- TODO proper idiom for this kind of test
    errLooksRight e@(Err { errName = n, errMsg = ErrMsg m})
      = n2fp n == badName && "permission denied" `isInfixOf` m
    errLooksRight _ = False
