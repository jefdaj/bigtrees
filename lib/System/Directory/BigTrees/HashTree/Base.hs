{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module System.Directory.BigTrees.HashTree.Base where

import Control.DeepSeq (NFData)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as BS
import Data.Char (toLower)
import Data.List (delete, find, nubBy, partition, sort, sortBy)
import Data.Store (Store (..))
import GHC.Generics (Generic)
import System.Directory.BigTrees.FilePath (fp2n, n2fp, pathComponents)
import System.Directory.BigTrees.Hash (Hash (unHash), hashBytes)
import System.Directory.BigTrees.HashLine (HashLine (..), IndentLevel (..), TreeType (..))
import System.Directory.BigTrees.Name (Name (..))
import System.Info (os)
import Test.QuickCheck (Arbitrary (..), Gen, Property, choose, resize, suchThat)
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)
import TH.Derive (Deriving, derive)

-- for comparing two trees without getting hung up on different overall names
-- TODO when was this needed?
renameRoot :: FilePath -> ProdTree -> ProdTree
renameRoot newName tree = tree { name = fp2n newName }

-- for removing duplicate filenames using nubBy, taking into account
-- case-insensitivity on apple filesystem
duplicateFilenames :: HashTree a -> HashTree a -> Bool
duplicateFilenames = if os == "darwin" then macDupes else unixDupes
  where
    macDupes  a b = map toLower (n2fp $ name a)
                 == map toLower (n2fp $ name b)
    unixDupes a b = n2fp (name a)
                 == n2fp (name b)

countFiles :: HashTree a -> Int
countFiles (File {}  )    = 1
countFiles (Dir  _ _ _ n) = n

hashContents :: [HashTree a] -> Hash
hashContents = hashBytes . B8.unlines . sort . map (BS.fromShort . unHash . hash)

{- A tree of file names matching (a subdirectory of) the annex,
 - where each dir and file node contains a hash of its contents.
 - TODO read and write files
 - TODO would also storing the number of files in each dir help, or timestamps?
 -}
-- data HashTree = DT.AnchoredDirTree Hash
--   deriving (Eq, Read, Show)
--   TODO rename name -> path?
data HashTree a
  = File
      { name     :: !Name
      , hash     :: !Hash
      , fileData :: !a
      }
  | Dir
      { name     :: !Name
      , hash     :: Hash
      , contents :: [HashTree a]
      , nFiles   :: Int
      }
  deriving (Generic, Ord, Read, Show)

-- We only need the file decoration for testing, so we can leave it off the production types
type ProdTree = HashTree ()

-- TODO disable this while testing to ensure deep equality?
instance Eq (HashTree a) where
  t1 == t2 = hash t1 == hash t2

-- TODO once there's also a dirData, should this be BiFunctor instead?
-- TODO should this also re-hash the file, or is that not part of the fileData idea?
instance Functor HashTree where
  fmap fn f@(File {}) = f { fileData = fn (fileData f) }
  fmap fn d@(Dir  {}) = d { contents = map (fmap fn) (contents d) }

-- TODO test functor identity law

instance NFData a => NFData (HashTree a)

-- https://hackage.haskell.org/package/store-0.7.2/docs/Data-Store-TH.html
$($(derive [d|
    instance Store a => Deriving (Store (HashTree a))
    |]))

instance Arbitrary TreeType where

  arbitrary = do
    n <- choose (0,1 :: Int)
    return $ [F, D] !! n

  -- you could shrink D -> F, but not without changing the rest of the hashline
  shrink _ = []

instance Arbitrary IndentLevel where
  arbitrary = IndentLevel <$> ((arbitrary :: Gen Int) `suchThat` (>= 0))
  shrink _ = []

instance Arbitrary Hash where
  arbitrary = fmap hashBytes (arbitrary :: Gen B8.ByteString)
  shrink _ = []

-- TODO can you really have an arbitrary hashline without the rest of a tree?
instance Arbitrary HashLine where

  arbitrary = do
    tt <- arbitrary :: Gen TreeType
    il <- arbitrary :: Gen IndentLevel
    h  <- arbitrary :: Gen Hash
    n  <- arbitrary :: Gen Name
    return $ HashLine (tt, il, h, n)

  -- only shrinks the filename
  shrink (HashLine (tt, il, h, n)) = map (\n' -> HashLine (tt, il, h, n')) (shrink n)

-- A HashTree where file contents are generated by QuickCheck and stored in
-- memory for round-trip tests
type TestTree = HashTree B8.ByteString

-- This is specialized to (HashTree B8.ByteString) because it needs to use the
-- same arbitrary bytestring for the file content and its hash
instance Arbitrary TestTree where

  arbitrary = do
    n <- arbitrary :: Gen Name
    -- TODO there's got to be a better way, right?
    i <- choose (0,5 :: Int)
    if i == 0

      then do
        !cs <- nubBy duplicateFilenames <$> resize 5 (arbitrary :: Gen [TestTree])
        return $ Dir { name     = n
                     , hash     = hashContents cs
                     , contents = cs
                     , nFiles   = sum $ map countFiles cs
                     }

      else do
        bs <- arbitrary :: Gen B8.ByteString
        return $ File { name = n
                      , hash = hashBytes bs
                      , fileData = bs
                      }

  -- only shrinks the filename
  shrink f@(File {}) = map (\n -> f { name = n }) (shrink $ name f)

  -- shrinks either the name or the contents, and adjusts the rest to match
  shrink d@(Dir {}) = newNames ++ newContents
    where
      newNames = map (\n -> d { name = n }) (shrink $ name d)
      newContents = map (\cs -> d { contents = cs
                                  , hash = hashContents cs
                                  , nFiles = sum $ map countFiles cs})
                        (shrink $ contents d)

-- TODO rename the actual function file -> fileData to match future dirData
-- TODO rewrite this in terms of a generic map/fold so it works with other types
dropFileData :: TestTree -> ProdTree
dropFileData d@(Dir {contents = cs}) = d {contents = map dropFileData cs}
dropFileData f@(File {})             = f {fileData = ()}


instance Arbitrary ProdTree where
  arbitrary = fmap dropFileData arbitrary

confirmFileHashes :: TestTree -> Bool
confirmFileHashes (File {fileData = f, hash = h}) = hashBytes f == h
confirmFileHashes (Dir {contents = cs})           = all confirmFileHashes cs

prop_confirm_file_hashes :: TestTree -> Bool
prop_confirm_file_hashes = confirmFileHashes


