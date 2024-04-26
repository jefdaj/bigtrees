{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module System.Directory.BigTrees.HashTree

  ( HashTree(..)
  , ProdTree

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
  , writeBinTree
  , writeTree

  -- for testing
  , countFiles
  , prop_roundtrip_prodtree_to_bin_hashes
  , prop_roundtrip_prodtree_to_bytestring
  , prop_roundtrip_prodtree_to_hashes
  , prop_roundtrip_testtree_to_dir

  )
  where

-- TODO would be better to adapt AnchoredDirTree with a custom node type than re-implement stuff


import Control.DeepSeq (force)
import qualified Data.ByteString.Char8 as B8
import qualified System.Directory as SD
import System.Directory.BigTrees.FilePath (n2fp)
import System.FilePath ((</>))
import System.FilePath.Glob (Pattern)
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import Test.QuickCheck (Arbitrary (..), Gen, Property)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)

import System.Directory.BigTrees.HashTree.Base (HashTree (..), ProdTree, TestTree, countFiles)
import System.Directory.BigTrees.HashTree.Build (buildProdTree, buildTree)
import System.Directory.BigTrees.HashTree.Edit (addSubTree, rmSubTree)
import System.Directory.BigTrees.HashTree.Read (accTrees, deserializeTree, readTestTree, readTree)
import System.Directory.BigTrees.HashTree.Search (dropTo, treeContainsHash, treeContainsPath)
import System.Directory.BigTrees.HashTree.Write (printTree, serializeTree, writeBinTree,
                                                 writeTestTreeDir, writeTree)

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
-- TODO write_tree_binary?
-- TODO flatten_tree

-- prop_roundtrip_prodtree_to_hashes ::

--     describe "HashTree" $ do
--       describe "HashTree" $ do
--         it "builds a tree from the test annex" $ pendingWith "need annex test harness"

-- TODO prop_confirm_dir_hashes too?

-- TODO round-trip to binary files too

-- TODO what's right here but wrong in the roundtrip to bytestring ones?
prop_roundtrip_prodtree_to_bytestring :: ProdTree -> Bool
prop_roundtrip_prodtree_to_bytestring t = t' == t
  where
    bs = B8.unlines $ serializeTree t -- TODO why didn't it include the unlines part again?
    t' = deserializeTree Nothing bs

roundTripProdTreeToHashes :: ProdTree -> IO ProdTree
roundTripProdTreeToHashes t =
  withSystemTempFile "roundtriptemp" $ \path hdl -> do
    hClose hdl
    writeTree path t
    readTree Nothing path

prop_roundtrip_prodtree_to_hashes :: Property
prop_roundtrip_prodtree_to_hashes = monadicIO $ do
  t1 <- pick arbitrary
  t2 <- run $ roundTripProdTreeToHashes t1
  assert $ t2 == t1

-- TODO separate thing for test and production trees here?
roundTripProdTreeToBinHashes :: ProdTree -> IO ProdTree
roundTripProdTreeToBinHashes t =
  withSystemTempFile "roundtriptemp" $ \path hdl -> do
    hClose hdl
    writeBinTree path t
    readTree Nothing path

prop_roundtrip_prodtree_to_bin_hashes :: Property
prop_roundtrip_prodtree_to_bin_hashes = monadicIO $ do
  t1 <- pick (arbitrary :: Gen ProdTree)
  t2 <- run $ roundTripProdTreeToBinHashes t1
  assert $ t2 == t1

-- the tests above round-trip to single files describing trees, whereas this
-- one round-trips to an actual directory tree on disk
-- note that you have to drop the bytestrings from the original testtree to compare them
roundTripTestTreeToDir :: TestTree -> IO TestTree
roundTripTestTreeToDir t =
  -- TODO is this not used?
  withSystemTempDirectory "roundtriptemp" $ \root -> do
    let tmpRoot = "/tmp/round-trip-tests" -- TODO replace with actual root
    SD.createDirectoryIfMissing True tmpRoot -- TODO False?
    let treePath = tmpRoot </> n2fp (name t)
    SD.removePathForcibly treePath -- TODO remove
    writeTestTreeDir tmpRoot t
    -- putStrLn $ "treePath: " ++ treePath
    readTestTree Nothing False [] treePath

prop_roundtrip_testtree_to_dir :: Property
prop_roundtrip_testtree_to_dir = monadicIO $ do
  t1 <- pick arbitrary
  t2 <- run $ roundTripTestTreeToDir t1
  assert $ force t2 == t1 -- force evaluation to prevent any possible conflicts
