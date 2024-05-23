{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Use camelCase" #-}

module System.Directory.BigTrees.HashTree

  ( HashTree(..)
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
  , printTreePaths

  -- for testing
  , countFiles
  , roundtripTestTreeToDir
  , dropFileData
  , writeTestTreeDir
  , prop_roundtrip_ProdTree_to_ByteString
  , prop_roundtrip_ProdTree_to_hashes
  , prop_roundtrip_TestTree_to_dir

  )
  where

-- TODO would be better to adapt AnchoredDirTree with a custom node type than re-implement stuff


-- import Control.DeepSeq (force)
import qualified Data.ByteString.Char8 as B8
import qualified System.Directory as SD
import System.Directory.BigTrees.Name (n2fp)
import System.FilePath ((</>))
import System.FilePath.Glob (Pattern)
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import Test.QuickCheck (Arbitrary (..), Property, arbitrary)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)

import System.Directory.BigTrees.HashTree.Base (HashTree (..), ProdTree, TestTree, countFiles,
                                                dropFileData)
import System.Directory.BigTrees.HashTree.Build (buildProdTree, buildTree)
import System.Directory.BigTrees.HashTree.Edit (addSubTree, rmSubTree)
import System.Directory.BigTrees.HashTree.Find (printTreePaths)
import System.Directory.BigTrees.HashTree.Read (accTrees, deserializeTree, readTestTree, readTree)
import System.Directory.BigTrees.HashTree.Search (dropTo, treeContainsHash, treeContainsPath)
import System.Directory.BigTrees.HashTree.Write (printTree, serializeTree, writeTestTreeDir,
                                                 writeTree)

-- import System.Directory.BigTrees.Util (absolutePath)

-- import qualified Data.ByteString.Char8 as B
-- import Text.Pretty.Simple (pPrint)


-- If passed a file this assumes it contains hashes and builds a tree of them;
-- If passed a dir it will scan it first and then build the tree.
-- TODO don't assume??
readOrBuildTree :: Bool -> Maybe Int -> [Pattern] -> FilePath -> IO ProdTree
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
roundtripTestTreeToDir :: TestTree -> IO TestTree
roundtripTestTreeToDir t =
  -- TODO is this not used?
  withSystemTempDirectory "bigtrees" $ \tmpDir -> do
    -- let tmpRoot = tmpDir </> "round-trip-tests" -- TODO use root
    -- SD.createDirectoryIfMissing True tmpDir -- TODO False?
    -- SD.removePathForcibly tmpDir -- TODO remove

    -- This is a little confusing, but the FilePath here should be the *parent*
    -- within which to write the root tree dir...
    writeTestTreeDir tmpDir t

    -- ... but then when reading it back in we need the full path including the
    -- root tree dir name.
    let treeRootDir = tmpDir </> n2fp (name t) -- TODO use IsName here
    readTestTree Nothing False [] treeRootDir

prop_roundtrip_TestTree_to_dir :: Property
prop_roundtrip_TestTree_to_dir = monadicIO $ do
  t1 <- pick arbitrary
  t2 <- run $ roundtripTestTreeToDir t1
  assert $ t2 == t1 -- force evaluation to prevent any possible conflicts
