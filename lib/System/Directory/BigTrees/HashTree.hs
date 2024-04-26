{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

-- TODO when you don't add an export list, does it not re-export everything?

module System.Directory.BigTrees.HashTree where
  -- ( HashTree(..)
  -- , ProdTree(..)
  -- , HashLine(..)
  -- , keepPath
  -- , readTree
  -- , buildTree
  -- , buildProdTree
  -- , readOrBuildTree
  -- , renameRoot
  -- , printTree
  -- , writeBinTree
  -- , serializeTree
  -- , writeTree
  -- , flattenTree
  -- , deserializeTree
  -- , hashContents
  -- , dropTo
  -- , treeContainsPath
  -- , treeContainsHash
  -- , addSubTree
  -- , rmSubTree
  -- , accTrees -- TODO hide this better?
  -- -- for testing
  -- , countFiles
  -- )
  -- where

-- TODO would be better to adapt AnchoredDirTree with a custom node type than re-implement stuff


import Control.DeepSeq (force)
import Control.Exception.Safe (catchAny)
import Control.Monad (msum, when)
import qualified Control.Monad.Parallel as P
import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as BS
import Data.Char (toLower)
import Data.Either (fromRight)
import Data.Function (on)
import Data.List (delete, find, nubBy, partition, sort, sortBy)
import Data.Maybe (isJust)
import Data.Store (decodeIO, encode)
import qualified System.Directory as SD
import System.Directory.BigTrees.FilePath (fp2n, n2fp, pathComponents)
import System.Directory.BigTrees.Hash (Hash, hashBytes)
import System.Directory.BigTrees.HashLine (HashLine (..), IndentLevel (IndentLevel),
                                           TreeType (D, F), lineP, prettyHashLine)
import System.Directory.BigTrees.Name (Name (..))
import qualified System.Directory.Tree as DT
import System.FilePath (joinPath, splitPath, (</>))
import System.FilePath.Glob (MatchOptions (..), Pattern, matchWith)
import System.Info (os)
import System.IO (IOMode (..), hClose, hFlush, stdout, withFile)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import Test.QuickCheck (Arbitrary (..), Gen, Property, choose, resize)
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)

import System.Directory.BigTrees.HashTree.Base (HashTree (Dir, File, contents, fileData, hash, nFiles, name),
                                                ProdTree, TestTree, countFiles, hashContents)
import System.Directory.BigTrees.HashTree.Build (buildProdTree, buildTree)
import System.Directory.BigTrees.HashTree.Read (deserializeTree, readTree, readTestTree)
import System.Directory.BigTrees.HashTree.Write
import System.Directory.BigTrees.HashTree.Search
import System.Directory.BigTrees.HashTree.Edit

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
