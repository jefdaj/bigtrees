{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Directory.BigTrees.HashTree.Base where

import Control.DeepSeq (NFData)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as BS
import Data.Char (toLower)
import Data.List (nubBy, sort)
import GHC.Generics (Generic)
import System.Directory.BigTrees.Hash (Hash (unHash), hashBytes)
import System.Directory.BigTrees.HashLine (HashLine (..), Depth (..), TreeType (..), ModTime(..), NBytes(..), bsBytes, NNodes(..), ErrMsg(..))
import System.Directory.BigTrees.Name (Name (..), fp2n, n2fp)
import System.Info (os)
import Test.QuickCheck (Arbitrary (..), Gen, choose, resize, sized, suchThat)
import TH.Derive (Deriving, derive)
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)

-- import Debug.Trace

-- for comparing two trees without getting hung up on different overall names
-- TODO when was this needed?
renameRoot :: FilePath -> ProdTree -> ProdTree
renameRoot newName tree = tree { nodeData = nd' }
  where
    nd' = (nodeData tree) { name = fp2n newName }

-- for removing duplicate filenames using nubBy, taking into account
-- case-insensitivity on apple filesystem
duplicateNames :: HashTree a -> HashTree a -> Bool
duplicateNames = if os == "darwin" then macDupes else unixDupes
  where
    macDupes  a b = map toLower (n2fp $ name $ nodeData a)
                 == map toLower (n2fp $ name $ nodeData b)
    unixDupes a b = n2fp (name $ nodeData a)
                 == n2fp (name $ nodeData b)

-- TODO Integer? not sure how big it could get
sumNodes :: HashTree a -> NNodes
sumNodes (File {}) = NNodes 1
sumNodes (Dir {nNodes=n}) = n -- this includes 1 for the dir itself

-- TODO is this needed, or will the fields be total?
-- TODO size unit
-- totalNBytes :: HashTree a -> Integer
-- totalNBytes (File {}) = undefined -- TODO add size field
-- totalNBytes (Dir  {}) = undefined -- TODO add size field

-- TODO is this needed?
-- totalModTime :: HashTree a -> Integer
-- totalModTime (File {}) = undefined -- TODO add mod time field
-- totalModTime (Dir  {}) = undefined -- TODO add mod time field

hashContents :: [HashTree a] -> Hash
hashContents = hashBytes . B8.unlines . sort . map (BS.fromShort . unHash . hash . nodeData)

-- TODO separate module for NodeData

-- All the fields shat should be common to File + Dir constructors
-- TODO come up with a better name?
-- TODO is it OK to make the entire thing strict for File but not Dir?
-- TODO should Eq be based only on the hash, or also the rest of it? 
data NodeData = NodeData
  { name     :: !Name
  , hash     :: !Hash
  , modTime  :: !ModTime
  , nBytes   :: !NBytes
  }
  deriving (Eq, Ord, Read, Show, Generic)

instance NFData NodeData

{- A tree of file names matching (a subdirectory of) the annex,
 - where each dir and file node contains a hash of its contents.
 - TODO read and write files
 - TODO would also storing the number of files in each dir help, or timestamps?
 -}
-- data HashTree = DT.AnchoredDirTree Hash
--   deriving (Eq, Read, Show)
--   TODO rename name -> path?
--   TODO make safe access fns and don't export the partial constructors
data HashTree a
  = Err
      { errName :: !Name   -- TODO NodeData?
      , errMsg  :: !ErrMsg -- TODO ByteString?
      }
  | File
      { nodeData :: !NodeData
      , fileData :: !a
      }
  | Dir
      { nodeData    :: NodeData
      , nNodes      :: NNodes -- TODO Integer? include in tree files
      , dirContents :: [HashTree a] -- TODO rename dirContents?
      }
  deriving (Generic, Ord, Read, Show)

-- TODO should this be a lens or something? going to want a setter too at some point
treeName :: HashTree a -> Name
treeName (Err  {errName =n }) = n
treeName (File {nodeData=nd}) = name nd
treeName (Dir  {nodeData=nd}) = name nd

-- We only need the file decoration for testing, so we can leave it off the production types
type ProdTree = HashTree ()

-- TODO disable this while testing to ensure deep equality?
-- TODO should this include mod time, or do we want to ignore it?
instance Eq (HashTree a) where
  (==) :: HashTree a -> HashTree a -> Bool
  t1@(Err {}) == t2@(Err {}) = errName t1 == errName t2 && errMsg t1 == errMsg t2
  t1@(Err {}) == _ = False
  _ == t2@(Err {}) = False
  t1 == t2 = (hash . nodeData) t1 == (hash . nodeData) t2

-- TODO once there's also a dirData, should this be BiFunctor instead?
-- TODO should this also re-hash the file, or is that not part of the fileData idea?
instance Functor HashTree where
  fmap :: (a -> b) -> HashTree a -> HashTree b
  fmap fn e@(Err  {}) = Err { errMsg = errMsg e, errName = errName e}
  fmap fn f@(File {}) = f { fileData = fn (fileData f) }
  fmap fn d@(Dir  {}) = d { dirContents = map (fmap fn) (dirContents d) }

-- TODO test functor identity law

instance NFData a => NFData (HashTree a)

-- A HashTree where file dirContents are generated by QuickCheck and stored in
-- memory for round-trip tests
type TestTree = HashTree B8.ByteString

-- Given a size "budget", generate test directory dirContents
-- TODO write this using a fold with accumulator? wait, maybe no need
-- this should have sum of nNodes == size... or is it nNodes-1?
-- TODO test prop for that
arbitraryContents :: Int -> Gen [TestTree]
arbitraryContents arbsize = arbitraryContentsHelper arbsize `suchThat` uniqNames
  where
    uniqNames cs = cs == nubBy duplicateNames cs

arbitraryContentsHelper :: Int -> Gen [TestTree]
arbitraryContentsHelper arbsize
  | arbsize <  1 = return []
  | arbsize == 1 = arbitraryFile >>= \t -> return [t] -- TODO clean this up
  | otherwise = do
      recNBytes <- choose (1,arbsize) -- TODO bias this to be smaller?
      let remNBytes = arbsize - recNBytes
      (recTree :: TestTree) <- resize recNBytes arbitrary
      arbitraryContents remNBytes >>= \cs -> return $ recTree:cs -- TODO clean this up

-- TODO does forAll add anything here that I'm not already getting from sized?
prop_arbitraryContents_length_matches_nNodes :: Gen Bool
prop_arbitraryContents_length_matches_nNodes =
  sized $ \arbsize -> do
    cs <- arbitraryContents arbsize
    let (NNodes total) = sum $ map sumNodes cs
        res = total == arbsize
    -- This verifies that it gets called with the full range of sizes:
    -- return $ traceShow ((size, sumFiles)) res
    return res

-- TODO make this explicit? it's the same as the overall Arbitrary instance
-- arbitraryTree :: Int -> Gen TestTree

arbitraryErr :: Gen TestTree
arbitraryErr = do
  n <- arbitrary :: Gen Name
  m <- arbitrary :: Gen ErrMsg
  return $ Err
    { errName = n
    , errMsg = m
    }

-- size == nNodes, so a file is always sized 1
-- TODO should these all be strict?
arbitraryFile :: Gen TestTree
arbitraryFile = do
  n  <- arbitrary :: Gen Name
  bs <- arbitrary :: Gen B8.ByteString
  mt <- arbitrary :: Gen ModTime
  return $ File
    { fileData = bs
    , nodeData = NodeData
      { name = n
      , hash = hashBytes bs
      , modTime = mt
      , nBytes = bsBytes bs -- TODO is this true on all platforms/architectures?
      }
    }

arbitraryDirSized :: Int -> Gen TestTree
arbitraryDirSized arbsize = do
  n  <- arbitrary :: Gen Name
  -- !cs <- nubBy duplicateNames <$> resize (s `div` 2) (arbitrary :: Gen [TestTree])
  -- TODO put back the nubBy part!
  !cs <- arbitraryContents arbsize -- TODO (s-1)?
  !mt <- arbitrary :: Gen ModTime
  !s <- fmap NBytes $ return 4096 -- TODO does dir size vary?
  -- TODO assert that nNodes == s here?
  return $ Dir
    { dirContents = cs
    , nNodes = sum $ (NNodes 1) : map sumNodes cs
    , nodeData = NodeData
      { name     = n
      , hash     = hashContents cs
      , modTime  = mt
      , nBytes   = sum $ s : map (nBytes .nodeData) cs
      }
    }

-- This is specialized to (HashTree B8.ByteString) because it needs to use the
-- same arbitrary bytestring for the file content and its hash
instance Arbitrary TestTree where

  arbitrary :: Gen TestTree
  arbitrary = sized $ \arbsize -> do

    -- TODO should `Err`s be one of the choices here?

    if arbsize < 2 -- TODO can it go below 1?
      then arbitraryFile
      else arbitraryDirSized arbsize

    -- n <- arbitrary :: Gen Name
    -- TODO there's got to be a better way, right?
    -- i <- choose (0,5 :: Int)
    -- if i == 0
      -- then arbitraryDirSized s
      -- else arbitraryFile

 -- only shrinks the filename
  shrink :: TestTree -> [TestTree]
  shrink f@(File {nodeData=nd}) = map (\n -> f { nodeData = nd {name = n} }) (shrink $ name nd)
  shrink e@(Err {}) = map (\n -> e { errName = n }) (shrink $ errName e)

  -- shrinks either the name or the dirContents, and adjusts the rest to match
  -- TODO any need to recurse manually into dirContents?
  shrink d@(Dir {nodeData=nd}) = newContents ++ newNames
    where
      newNames = map (\n -> d { nodeData = nd { name = n } }) (shrink $ name nd)
      newContents = map (\cs -> d { dirContents = cs
                                  , nodeData = nd {hash = hashContents cs}
                                  , nNodes = sum $ 1 : map sumNodes cs}) -- TODO factor out
                        (shrink $ dirContents d)

-- TODO rename the actual function file -> fileData to match future dirData
-- TODO rewrite this in terms of a generic map/fold so it works with other types
dropFileData :: HashTree a -> ProdTree
dropFileData d@(Dir {dirContents = cs}) = d {dirContents = map dropFileData cs}
dropFileData f@(File {})             = f {fileData = ()}
dropFileData e@(Err  {})             = Err { errName = errName e, errMsg = errMsg e }

instance Arbitrary ProdTree where
  arbitrary :: Gen ProdTree
  arbitrary = fmap dropFileData (arbitrary :: Gen TestTree)

confirmFileHashes :: TestTree -> Bool
confirmFileHashes (File {fileData = f, nodeData=nd}) = hashBytes f == hash nd
confirmFileHashes (Dir {dirContents = cs})           = all confirmFileHashes cs
confirmFileHashes (Err {})                           = True -- TODO False?

prop_confirm_file_hashes :: TestTree -> Bool
prop_confirm_file_hashes = confirmFileHashes
