{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module System.Directory.BigTrees.HashTree.Base where

import Control.DeepSeq (NFData)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as BS
import Data.Char (toLower)
import Data.List (nubBy, sort)
import GHC.Generics (Generic)
import System.Directory.BigTrees.Hash (Hash (unHash), hashBytes)
import System.Directory.BigTrees.HashLine (HashLine (..), IndentLevel (..), TreeType (..))
import System.Directory.BigTrees.Name (Name (..), fp2n, n2fp)
import System.Info (os)
import Test.QuickCheck (Arbitrary (..), Gen, choose, resize, suchThat, sized, forAll)
import Test.QuickCheck.Instances.ByteString ()
import TH.Derive (Deriving, derive)

import Debug.Trace

-- for comparing two trees without getting hung up on different overall names
-- TODO when was this needed?
renameRoot :: FilePath -> ProdTree -> ProdTree
renameRoot newName tree = tree { name = fp2n newName }

-- for removing duplicate filenames using nubBy, taking into account
-- case-insensitivity on apple filesystem
duplicateNames :: HashTree a -> HashTree a -> Bool
duplicateNames = if os == "darwin" then macDupes else unixDupes
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
  (==) :: HashTree a -> HashTree a -> Bool
  t1 == t2 = hash t1 == hash t2

-- TODO once there's also a dirData, should this be BiFunctor instead?
-- TODO should this also re-hash the file, or is that not part of the fileData idea?
instance Functor HashTree where
  fmap :: (a -> b) -> HashTree a -> HashTree b
  fmap fn f@(File {}) = f { fileData = fn (fileData f) }
  fmap fn d@(Dir  {}) = d { contents = map (fmap fn) (contents d) }

-- TODO test functor identity law

instance NFData a => NFData (HashTree a)

instance Arbitrary TreeType where

  --  TODO oneof
  arbitrary :: Gen TreeType
  arbitrary = do
    n <- choose (0,1 :: Int)
    return $ [F, D] !! n

  -- you could shrink D -> F, but not without changing the rest of the hashline
  shrink :: TreeType -> [TreeType]
  shrink _ = []

-- TODO remove?
instance Arbitrary IndentLevel where

  arbitrary :: Gen IndentLevel
  arbitrary = IndentLevel <$> ((arbitrary :: Gen Int) `suchThat` (>= 0))

  shrink :: IndentLevel -> [IndentLevel]
  shrink _ = []

-- TODO remove?
instance Arbitrary Hash where

  arbitrary :: Gen Hash
  arbitrary = fmap hashBytes (arbitrary :: Gen B8.ByteString)

  shrink :: Hash -> [Hash]
  shrink _ = []

-- TODO can you really have an arbitrary hashline without the rest of a tree?
-- TODO remove?
instance Arbitrary HashLine where

  arbitrary :: Gen HashLine
  arbitrary = do
    tt <- arbitrary :: Gen TreeType
    il <- arbitrary :: Gen IndentLevel
    h  <- arbitrary :: Gen Hash
    n  <- arbitrary :: Gen Name
    return $ HashLine (tt, il, h, n)

  -- only shrinks the filename
  shrink :: HashLine -> [HashLine]
  shrink (HashLine (tt, il, h, n)) = map (\n' -> HashLine (tt, il, h, n')) (shrink n)

-- A HashTree where file contents are generated by QuickCheck and stored in
-- memory for round-trip tests
type TestTree = HashTree B8.ByteString

-- Given a size "budget", generate test directory contents
-- TODO write this using a fold with accumulator? wait, maybe no need
-- this should have sum of nFiles == size... or is it nFiles-1?
-- TODO test prop for that
arbitraryContents :: Int -> Gen [TestTree]
arbitraryContents size = arbitraryContentsHelper size `suchThat` uniqNames
  where
    uniqNames cs = cs == nubBy duplicateNames cs

arbitraryContentsHelper :: Int -> Gen [TestTree]
arbitraryContentsHelper size
  | size <  1 = return []
  | size == 1 = arbitraryFile >>= \t -> return [t] -- TODO clean this up
  | otherwise = do
      recSize <- choose (1,size) -- TODO bias this to be smaller?
      let remSize = size - recSize
      (recTree :: TestTree) <- resize recSize arbitrary
      arbitraryContents remSize >>= \cs -> return $ recTree:cs -- TODO clean this up

-- https://stackoverflow.com/a/29107066
-- TODO what's a reasonable upper bound on the sizes here?
-- TODO can I used sized with this? would be cool
-- prop_arbitrary_contents_length_matches_nFiles :: Int -> Bool
prop_arbitrary_contents_length_matches_nFiles =
  forAll (choose (0, 10)) $ \size -> do
    cs <- arbitraryContents size
    let sumFiles = sum $ map countFiles cs
        res = sumFiles == size
    return $ if res then res else traceShow ((size, sumFiles, cs)) res

-- TODO make this explicit? it's the same as the overall Arbitrary instance
-- arbitraryTree :: Int -> Gen TestTree

-- size == nFiles, so a file is always sized 1
arbitraryFile :: Gen TestTree
arbitraryFile = do
  n  <- arbitrary :: Gen Name
  bs <- arbitrary :: Gen B8.ByteString
  return $ File
    { name = n
    , hash = hashBytes bs
    , fileData = bs
    }

arbitraryDirSized :: Int -> Gen TestTree
arbitraryDirSized size = do
  n  <- arbitrary :: Gen Name
  -- !cs <- nubBy duplicateNames <$> resize (s `div` 2) (arbitrary :: Gen [TestTree])
  -- TODO put back the nubBy part!
  !cs <- arbitraryContents size -- TODO (s-1)?
  -- TODO assert that nFiles == s here?
  return $ Dir
    { name     = n
    , hash     = hashContents cs
    , contents = cs
    , nFiles   = sum $ map countFiles cs
    }

-- This is specialized to (HashTree B8.ByteString) because it needs to use the
-- same arbitrary bytestring for the file content and its hash
instance Arbitrary TestTree where

  arbitrary :: Gen TestTree
  arbitrary = sized $ \size -> do
    if size < 2 -- TODO can it go below 1?
      then arbitraryFile
      else arbitraryDirSized size

    -- n <- arbitrary :: Gen Name
    -- TODO there's got to be a better way, right?
    -- i <- choose (0,5 :: Int)
    -- if i == 0
      -- then arbitraryDirSized s
      -- else arbitraryFile

 -- only shrinks the filename
  shrink :: TestTree -> [TestTree]
  shrink f@(File {}) = map (\n -> f { name = n }) (shrink $ name f)

  -- shrinks either the name or the contents, and adjusts the rest to match
  shrink d@(Dir {}) = newNames ++ newContents
    where
      newNames = map (\n -> d { name = n }) (shrink $ name d)
      newContents = map (\cs -> d { contents = cs
                                  , hash = hashContents cs
                                  , nFiles = sum $ map countFiles cs}) -- TODO +1?
                        (shrink $ contents d)

-- TODO rename the actual function file -> fileData to match future dirData
-- TODO rewrite this in terms of a generic map/fold so it works with other types
dropFileData :: HashTree a -> ProdTree
dropFileData d@(Dir {contents = cs}) = d {contents = map dropFileData cs}
dropFileData f@(File {})             = f {fileData = ()}


instance Arbitrary ProdTree where
  arbitrary :: Gen ProdTree
  arbitrary = fmap dropFileData (arbitrary :: Gen TestTree)

confirmFileHashes :: TestTree -> Bool
confirmFileHashes (File {fileData = f, hash = h}) = hashBytes f == h
confirmFileHashes (Dir {contents = cs})           = all confirmFileHashes cs

prop_confirm_file_hashes :: TestTree -> Bool
prop_confirm_file_hashes = confirmFileHashes
