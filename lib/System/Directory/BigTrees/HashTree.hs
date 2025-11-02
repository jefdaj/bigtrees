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
  , treeNNodes
  , treeEqIgnoringModTime

  -- for testing
  , roundtripTestTreeToActualTmpdir
  , roundtripProdTreeToBigtreeFile
  , dropFileData
  , writeTestTreeDir
  , isErr
  , treeEqIgnoringModTime
  -- TODO fix failing assertions:
  , prop_roundtrip_ProdTree_to_ByteString
  , prop_roundtrip_ProdTree_to_bigtree_file
  , prop_roundtrip_TestTree_to_actual_tmpdir
  , unit_tree_from_bad_path_is_Err
  , unit_roundtrip_Err_to_bigtree_file
  , unit_buildProdTree_catches_permission_error
  , bench_roundtrip_ProdTree_to_bigtree_file

  )
  where

-- TODO would be better to adapt AnchoredDirTree with a custom node type than re-implement stuff

import Control.DeepSeq (deepseq, force)

import qualified Data.ByteString.Char8 as B8
import System.Directory.BigTrees.HashLine (ErrMsg (..))
import System.Directory.BigTrees.Name (Name (..))
import qualified System.Directory.OsPath as SDO
import System.OsPath (OsPath, encodeFS, osp, (</>))
-- import System.FilePath.Glob (Pattern)
import Control.Exception (evaluate)
import Control.Exception.Safe (SomeException, try)
import Control.Monad (unless)
import qualified System.FilePath as SF
import System.IO (IOMode (..), hClose, withBinaryFile)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)

import qualified Control.Concurrent.Thread.Delay as D
import qualified Data.Knob as K
import Data.List (isInfixOf)
import System.Directory.BigTrees.HashTree.Base
import System.Directory.BigTrees.HashTree.Build (buildProdTree, buildTree)
import System.Directory.BigTrees.HashTree.Edit (addSubTree, rmSubTree)
import System.Directory.BigTrees.HashTree.Find (listTreePaths)
import System.Directory.BigTrees.HashTree.Read (accTrees, hReadTree, readLastHashLineAndFooter,
                                                readTestTree, readTree)
import System.Directory.BigTrees.HashTree.Search (SearchConfig (..), dropTo, emptySearchConfig,
                                                  treeContainsHash, treeContainsPath)
import System.Directory.BigTrees.HashTree.Write (hWriteTree, printTree, serializeTree,
                                                 writeTestTreeDir, writeTree)
import System.Directory.BigTrees.Logging (LogCfg (..), addLogContext, die)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (cwd, proc, readCreateProcess)
import qualified Test.HUnit as HU

import System.Directory.BigTrees.Util (propertyWithExceptions)

-- import qualified Data.ByteString.Char8 as B
import Text.Pretty.Simple (pPrint)

-- If passed a file this assumes it contains hashes and builds a tree of them;
-- If passed a dir it will scan it first and then build the tree.
-- TODO don't assume??
readOrBuildTree :: SearchConfig -> LogCfg -> OsPath -> IO ProdTree
readOrBuildTree cfg lCfg path = do
  isDir  <- SDO.doesDirectoryExist path
  isFile <- SDO.doesFileExist      path
  if      isFile then readTree cfg lCfg path
  else if isDir then buildProdTree cfg lCfg path
  else die (addLogContext lCfg "readOrBuildTree") $ B8.pack $ "No such file: " ++ show path

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

-- TODO fix failing assertion
prop_roundtrip_ProdTree_to_ByteString :: Property
prop_roundtrip_ProdTree_to_ByteString = monadicIO $ do
  knob <- K.newKnob mempty
  (t1 :: ProdTree) <- pick arbitrary
  let cfg = emptySearchConfig
  K.withFileHandle knob "knob" WriteMode $ \h -> hWriteTree cfg NoLog h t1 -- TODO hClose?
  -- run $ withBinaryFile "/tmp/proptest1.bigtree" WriteMode $ \h -> hWriteTree cfg NoLog h t1 -- TODO hClose?
  t2 <- run $ K.withFileHandle knob "knob" ReadMode $ hReadTree cfg NoLog 4096

  -- TODO looks like once it's written once, it works. so issue is with Arbitrary instance?
  unless (t1 == t2) $ do
    run $ writeTree cfg NoLog [osp|/tmp/roundtrip-t1.bigtree|] t1
    run $ writeTree cfg NoLog [osp|/tmp/roundtrip-t2.bigtree|] t2

  --   run $ print t1
  --   run $ print t2
  assert $ t2 == t1

bench_roundtrip_ProdTree_to_bigtree_file :: Int -> IO ()
bench_roundtrip_ProdTree_to_bigtree_file n = do
  (t1 :: ProdTree) <- generate $ resize n arbitrary
  t2 <- roundtripProdTreeToBigtreeFile t1
  -- assert $ t2 == t1 -- TODO why aren't we asserting this? because it's a benchmark?
  return $ deepseq t2 ()

-- TODO unify with the knob version above
roundtripProdTreeToBigtreeFile :: ProdTree -> IO ProdTree
roundtripProdTreeToBigtreeFile t =
  withSystemTempFile "bigtrees" $ \path hdl -> do
    path' <- encodeFS path
    hClose hdl
    let cfg = emptySearchConfig
    writeTree cfg NoLog path' t -- TODO exclude defaultConfig?
    -- TODO come up with a better way to inspect intermediate versions here
    -- SDO.copyFile path' [osp|/tmp/roundtripfail.bigtree|]
    readTree cfg NoLog path'

-- TODO fix failing assertion
prop_roundtrip_ProdTree_to_bigtree_file :: Property
prop_roundtrip_ProdTree_to_bigtree_file = monadicIO $ do
  t1 <- pick arbitrary
  t2 <- run $ roundtripProdTreeToBigtreeFile t1
  assert $ t2 == t1

-- the tests above round-trip to single files describing trees, whereas this
-- one round-trips to an actual directory tree on disk
-- note that you have to drop the bytestrings from the original testtree to compare them
roundtripTestTreeToActualTmpdir :: LogCfg -> TestTree -> IO TestTree
roundtripTestTreeToActualTmpdir lCfg tree =

  withSystemTempDirectory "bigtrees" $ \tmpDir -> do
    ospTmpDir <- encodeFS tmpDir
    let treeRootDir = ospTmpDir </> (unName . treeName) tree
    writeTestTreeDir lCfg treeRootDir tree
    tree' <- (renameRoot $ treeName tree) <$> readTestTree emptySearchConfig lCfg treeRootDir

    -- This prevents a race condition between reading the tree and cleaning up the tmpdir
    evaluate $ force tree'

prop_roundtrip_TestTree_to_actual_tmpdir :: Property
prop_roundtrip_TestTree_to_actual_tmpdir =
  propertyWithExceptions
    (roundtripTestTreeToActualTmpdir NoLog)
    treeEqIgnoringModTime

unit_tree_from_bad_path_is_Err :: HU.Assertion
unit_tree_from_bad_path_is_Err =
  withSystemTempDirectory "bigtrees" $ \tmpDir -> do
    tmpDir' <- encodeFS tmpDir
    let badPath = tmpDir' </> [osp|doesnotexist|]
    tree <- buildProdTree emptySearchConfig NoLog badPath
    HU.assertBool "tree built from non-existent path should be Err" $ isErr tree

unit_roundtrip_Err_to_bigtree_file :: HU.Assertion
unit_roundtrip_Err_to_bigtree_file = do
  withSystemTempDirectory "bigtrees" $ \tmpDir -> do
    tmpDir' <- encodeFS tmpDir
    let badPath = tmpDir' </> [osp|doesnotexist|]
    t1 <- buildProdTree emptySearchConfig NoLog badPath
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
    t1 <- buildProdTree emptySearchConfig NoLog badPath'
    HU.assertBool "Err looks right" $ errLooksRight t1
  where
    badName = "file-without-read-permission.txt"
    -- TODO proper idiom for this kind of test
    errLooksRight e@(Err { errName = n, errMsg = ErrMsg m})
      = "permission denied" `isInfixOf` m -- TODO add back check for name == badName?
    errLooksRight _ = False

-- prop_roundtrip_debug :: Property
-- prop_roundtrip_debug =
--   forAllShrink arbitrary shrink $ \t1 ->
--     counterexample ("Testing with: " ++ show t1) $
--     ioProperty $ do
--       result <- try (roundtripTestTreeToActualTmpdir t1)
--       case result of
--         Left ex -> do
--           putStrLn $ "Exception: " ++ show (ex :: SomeException)
--           return False
--         Right t2 -> return $ treeEqIgnoringModTime t1 t2
