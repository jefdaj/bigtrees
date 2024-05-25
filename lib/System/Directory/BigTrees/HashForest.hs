{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}


{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE InstanceSigs       #-}

-- TODO are forests even needed?

module System.Directory.BigTrees.HashForest

  ( HashForest(..)
  , ProdForest
  , readTrees
  , buildForest
  , readForest
  , readOrBuildTrees
  , printForest
  , writeForest
  , printForestPaths

  -- tests
  , prop_roundtrip_HashForest_to_ByteString
  , prop_roundtrip_HashForest_to_hashes

  )
  where
import qualified Data.ByteString.Char8 as B8
import System.Directory.BigTrees.HashLine (parseHashLines)
import System.Directory.BigTrees.HashTree (readOrBuildTree)
import System.Directory.BigTrees.HashTree.Base (HashTree)
import System.Directory.BigTrees.HashTree.Build (buildProdTree)
import System.Directory.BigTrees.HashTree.Find (printTreePaths)
import System.Directory.BigTrees.HashTree.Read (accTrees, readTree)
import System.Directory.BigTrees.HashTree.Write (printTree, serializeTree)
-- import System.FilePath.Glob (Pattern)
import System.IO (IOMode (..), hClose, withFile)
import System.IO.Temp (withSystemTempFile)
import Test.QuickCheck (Arbitrary (..), Gen, Property, resize)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)
import TH.Derive ()

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

-- TODO how should errors propagate?
readTrees :: Maybe Int -> [FilePath] -> IO (HashForest ())
readTrees md paths = HashForest <$> mapM (readTree md) paths

readForest :: Maybe Int -> FilePath -> IO (HashForest ())
readForest md path = deserializeForest md <$> B8.readFile path

-- TODO how should errors propagate?
buildForest :: Bool -> [String] -> [FilePath] -> IO (HashForest ())
buildForest beVerbose excludes paths = HashForest <$> mapM (buildProdTree beVerbose excludes) paths

-- TODO be clearer: this works on trees, but you could also read a forest directly
readOrBuildTrees :: Bool -> Maybe Int -> [String] -> [FilePath] -> IO (HashForest ())
readOrBuildTrees vrb mmaxdepth excludes paths = HashForest <$> mapM (readOrBuildTree vrb mmaxdepth excludes) paths

-- TODO is there a reason this doesn't join lines?
serializeForest :: HashForest () -> [B8.ByteString]
serializeForest (HashForest ts) = concatMap serializeTree ts

deserializeForest :: Maybe Int -> B8.ByteString -> HashForest ()
deserializeForest md = HashForest <$> fmap snd . foldr accTrees [] . reverse . parseHashLines md

printForest :: HashForest () -> IO ()
printForest (HashForest ts) = mapM_ printTree ts

-- this uses a handle for streaming output, which turns out to be important for memory usage
-- TODO rename writeHashes? this is a confusing way to say that
writeForest :: FilePath -> HashForest () -> IO ()
writeForest path forest = withFile path WriteMode $ \h ->
  mapM_ (B8.hPutStrLn h) (serializeForest forest)

-- TODO sort the trees by name here? match however find does it
printForestPaths :: Maybe String -> String -> HashForest a -> IO ()
printForestPaths mRegex fmt (HashForest ts) = mapM_ (printTreePaths mRegex fmt) ts

-----------
-- tests --
-----------

type TestForest = HashForest B8.ByteString

instance Arbitrary TestForest where

  arbitrary :: Gen TestForest
  arbitrary = HashForest <$> resize 3 arbitrary

  shrink :: TestForest -> [TestForest]
  shrink (HashForest xs) = HashForest <$> shrink xs

instance Arbitrary ProdForest where

  arbitrary :: Gen ProdForest
  arbitrary = HashForest <$> arbitrary

  shrink :: ProdForest -> [ProdForest]
  shrink (HashForest ts) = HashForest <$> shrink ts

prop_roundtrip_HashForest_to_ByteString :: HashForest () -> Bool
prop_roundtrip_HashForest_to_ByteString t = t' == t
  where
    bs = B8.unlines $ serializeForest t -- TODO why didn't it include the unlines part again?
    t' = deserializeForest Nothing bs

roundtrip_HashForest_to_hashes :: HashForest () -> IO (HashForest ())
roundtrip_HashForest_to_hashes t =
  withSystemTempFile "bigtrees" $ \path hdl -> do
    hClose hdl
    writeForest path t
    readForest Nothing path

prop_roundtrip_HashForest_to_hashes :: Property
prop_roundtrip_HashForest_to_hashes = monadicIO $ do
  t1 <- pick arbitrary
  t2 <- run $ roundtrip_HashForest_to_hashes t1
  assert $ t2 == t1
