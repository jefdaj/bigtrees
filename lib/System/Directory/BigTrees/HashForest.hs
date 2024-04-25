{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

-- TODO are forests even needed?

module System.Directory.BigTrees.HashForest where
  -- ( HashForest(..)
  -- , ProdForest
  -- , readTrees
  -- , buildForest
  -- , readForest
  -- , readOrBuildTrees
  -- , serializeForest
  -- , deserializeForest
  -- , printForest
  -- , writeForest
  -- , writeBinForest
  -- )
  -- where

import Control.Exception.Safe (catchAny)
import qualified Data.ByteString.Char8 as B8
import Data.Store (Store (..), decodeIO, encode)
import System.Directory.BigTrees.HashLine (parseHashes)
import System.Directory.BigTrees.HashTree
    ( readOrBuildTree, serializeTree, printTree )
import System.Directory.BigTrees.HashTree.Build ( buildProdTree )
import System.Directory.BigTrees.HashTree.Read
    ( readTree, accTrees )
import System.Directory.BigTrees.HashTree.Types ( HashTree )
import System.FilePath.Glob (Pattern)
import System.IO (IOMode (..), hClose, withFile)
import System.IO.Temp ( withSystemTempFile )
import Test.QuickCheck ( Arbitrary(..), Property, resize )
import Test.QuickCheck.Monadic ( assert, monadicIO, pick, run )
import TH.Derive ( derive, Deriving )

{- A forest is just a list of trees without an overall content hash. It's used
 - at the top level when reading potentially more than one tree from the
 - command line.
 -}
newtype HashForest a
  = HashForest [HashTree a]
  -- deriving (Eq, Show, Read)

type ProdForest = HashForest ()

deriving instance Eq   a => Eq   (HashForest a)
deriving instance Show a => Show (HashForest a)
deriving instance Read a => Read (HashForest a)

-- https://hackage.haskell.org/package/store-0.7.2/docs/Data-Store-TH.html
-- TODO why can't you extend () to and type Store a => a here?
$($(derive [d|
    instance Deriving (Store (HashForest ()))
    |]))

-- TODO how should errors propagate?
readTrees :: Maybe Int -> [FilePath] -> IO (HashForest ())
readTrees md paths = HashForest <$> mapM (readTree md) paths

-- TODO remove existing bin case and only handle "txt" hashes now? (which will be partly binary)
readForest :: Maybe Int -> FilePath -> IO (HashForest ())
readForest md path = catchAny
                      (B8.readFile path >>= decodeIO)
                      (\_ -> deserializeForest md <$> B8.readFile path)

-- TODO how should errors propagate?
buildForest :: Bool -> [Pattern] -> [FilePath] -> IO (HashForest ())
buildForest beVerbose excludes paths = HashForest <$> mapM (buildProdTree beVerbose excludes) paths

-- TODO be clearer: this works on trees, but you could also read a forest directly
readOrBuildTrees :: Bool -> Maybe Int -> [Pattern] -> [FilePath] -> IO (HashForest ())
readOrBuildTrees vrb mmaxdepth excludes paths = HashForest <$> mapM (readOrBuildTree vrb mmaxdepth excludes) paths

-- TODO is there a reason this doesn't join lines?
serializeForest :: HashForest () -> [B8.ByteString]
serializeForest (HashForest ts) = concatMap serializeTree ts

deserializeForest :: Maybe Int -> B8.ByteString -> HashForest ()
deserializeForest md = HashForest <$> fmap snd . foldr accTrees [] . reverse . parseHashes md

printForest :: HashForest () -> IO ()
printForest (HashForest ts) = mapM_ printTree ts

-- this uses a handle for streaming output, which turns out to be important for memory usage
-- TODO rename writeHashes? this is a confusing way to say that
writeForest :: FilePath -> HashForest () -> IO ()
writeForest path forest = withFile path WriteMode $ \h ->
  mapM_ (B8.hPutStrLn h) (serializeForest forest)

writeBinForest :: FilePath -> HashForest () -> IO ()
writeBinForest path forest = B8.writeFile path $ encode forest

-----------
-- tests --
-----------

type TestForest = HashForest B8.ByteString

instance Arbitrary TestForest where
  arbitrary = HashForest <$> resize 3 arbitrary
  shrink (HashForest xs) = HashForest <$> shrink xs

instance Arbitrary ProdForest where
  arbitrary = HashForest <$> arbitrary
  shrink (HashForest ts) = HashForest <$> shrink ts

prop_roundtrip_hashforest_to_bytestring :: HashForest () -> Bool
prop_roundtrip_hashforest_to_bytestring t = t' == t
  where
    bs = B8.unlines $ serializeForest t -- TODO why didn't it include the unlines part again?
    t' = deserializeForest Nothing bs

roundtrip_hashforest_to_hashes :: HashForest () -> IO (HashForest ())
roundtrip_hashforest_to_hashes t = withSystemTempFile "roundtriptemp" $ \path hdl -> do
  hClose hdl
  writeForest path t
  readForest Nothing path

prop_roundtrip_hashforest_to_hashes :: Property
prop_roundtrip_hashforest_to_hashes = monadicIO $ do
  t1 <- pick arbitrary
  t2 <- run $ roundtrip_hashforest_to_hashes t1
  assert $ t2 == t1

roundtrip_hashforest_to_binary_hashes :: HashForest () -> IO (HashForest ())
roundtrip_hashforest_to_binary_hashes t = withSystemTempFile "roundtriptemp" $ \path hdl -> do
  hClose hdl
  writeBinForest path t
  readForest Nothing path

prop_roundtrip_hashforest_to_binary_hashes :: Property
prop_roundtrip_hashforest_to_binary_hashes = monadicIO $ do
  t1 <- pick arbitrary
  t2 <- run $ roundtrip_hashforest_to_binary_hashes t1
  assert $ t2 == t1
