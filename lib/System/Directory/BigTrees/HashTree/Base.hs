{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Directory.BigTrees.HashTree.Base where

import Control.DeepSeq (NFData)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as BS
import qualified Data.CaseInsensitive as CI
import Data.Char (toLower)
import Data.Function (on)
import Data.List (nubBy, sort, sortBy)
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import GHC.Generics (Generic)
import System.Directory.BigTrees.Hash (Hash (..), hashBytes)
import System.Directory.BigTrees.HashLine (Depth (..), ErrMsg (..), HashLine (..), LinkTarget,
                                           ModTime (..), NBytes (..), NNodes (..), TreeType (..),
                                           bsBytes)
import System.Directory.BigTrees.Name (Name (..), fp2n, n2bs)
import System.Info (os)
import System.OsPath (OsPath)
import Test.QuickCheck (Arbitrary (..), Gen, choose, resize, sized, suchThat)
import TH.Derive (Deriving, derive)

-- import Debug.Trace

-- for comparing two trees without getting hung up on different overall names
renameRoot :: Name -> HashTree a -> HashTree a
renameRoot newName e@(Err {}) = e { errName = newName }
renameRoot newName tree = tree { nodeData = nd' }
  where
    nd' = (nodeData tree) { name = newName }

-- for removing duplicate filenames using nubBy, taking into account
-- case-insensitivity on apple filesystem
-- TODO does CI.mk roughly match HFS+ case insensitivity?
duplicateNames :: HashTree a -> HashTree a -> Bool
duplicateNames = if os == "darwin" then macDupes else unixDupes
  where
    unixDupes a b = treeName a == treeName b
    macDupes a b = CI.mk (n2bs $ treeName a)
                == CI.mk (n2bs $ treeName b)

-- TODO Integer? not sure how big it could get
-- TODO rename treeNNodes for consistency
sumNodes :: HashTree a -> NNodes
sumNodes (Err  {})        = NNodes 1 -- TODO is this right?
sumNodes (File {})        = NNodes 1
sumNodes (Link {})        = NNodes 1 -- TODO is this right?
sumNodes (Graft {graftTree=t}) = NNodes 1 + sumNodes t
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

-- TODO handle Err case
hashContents :: [HashTree a] -> Hash
hashContents = hashBytes . B8.unlines . sort . map (BS.fromShort . unHash . treeHash)

-- TODO separate module for NodeData

-- All the fields shat should be common to File + Dir constructors.
-- Note that it's important NOT to make the hash, mod time, or nbytes strict in Dirs.
-- TODO strict name though?
-- TODO come up with a better name?
-- TODO should Eq be based only on the hash, or also the rest of it?
data NodeData = NodeData
  { name    :: Name
  , hash    :: Hash
  , modTime :: ModTime
  , nBytes  :: NBytes
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData NodeData

-- Whether or not a link points within the tree AND the content exists. If
-- it's "in the tree" in both senses, we use the target contents for the node
-- data; otherwise we assume they might change without us noticing and fall back
-- to treating the link itself as the data.
type LinkInTree = Bool

{- A tree of file names matching (a subdirectory of) the annex,
 - where each dir and file node contains a hash of its contents.
 - TODO make safe access fns and don't export the partial constructors
 - TODO store link dest even when broken in order to write to a dir
 -}
data HashTree a
  = Err
      { errName :: !Name   -- TODO NodeData?
      , errMsg  :: !ErrMsg -- TODO ByteString?
      }
  | File
      { nodeData :: !NodeData
      , fileData :: !a
      }
  | Link
      { nodeData   :: !NodeData
      , linkData   :: !(Maybe a)
      , linkInTree :: !LinkInTree
      , linkTarget :: !LinkTarget
      }
  | Dir
      { nodeData    :: NodeData
      , nNodes      :: NNodes -- TODO Integer? include in tree files
      , dirContents :: [HashTree a] -- TODO rename dirContents?
      }
  | Graft
      { graftName :: !Name
      , graftTree :: HashTree a -- TODO just one, right? not a list/forest
      }
  deriving (Eq, Ord, Show, Generic)

isErr :: forall a. HashTree a -> Bool
isErr (Err {}) = True
isErr _        = False

treeType :: HashTree a -> TreeType
treeType (Dir  {})                   = D
treeType (Err  {})                   = E
treeType (File {})                   = F
treeType (Link {linkData = Nothing}) = B
treeType (Link {})                   = L
treeType (Graft {})                  = G

-- TODO should this be a lens or something? going to want a setter too at some point
-- TODO return Maybe here?
treeName :: HashTree a -> Name
treeName (Err   {errName   = n}) = n
treeName (Graft {graftName = n}) = n
treeName t                       = name $ nodeData t

-- TODO is the handling of Err reasonable? think about it more
-- TODO return Maybe here?
treeModTime :: HashTree a -> ModTime
treeModTime (Err {}) = ModTime 0
treeModTime g@(Graft {graftTree=t}) = treeModTime t
treeModTime t        = modTime $ nodeData t

-- TODO return Maybe here?
treeNBytes :: HashTree a -> NBytes
treeNBytes (Err {}) = NBytes 0
treeNBytes (Graft {graftTree=t}) = treeNBytes t
treeNBytes t        = nBytes $ nodeData t

-- TODO return Maybe here?
treeHash :: HashTree a -> Hash
treeHash (Err {}) = Hash "ERROR" -- TODO is this reasonable? we do want it to change parent hash
treeHash (Graft {graftTree=t}) = treeHash t -- TODO is this right?? don't want to duplicate it
treeHash t        = hash $ nodeData t

-- We only need the file decoration for testing, so we can leave it off the production types
type ProdTree = HashTree ()

-- TODO when should this be used instead of regular derived (==)?
-- eqByHash :: HashTree a -> HashTree a -> Bool
-- eqByHash t1@(Err {}) t2@(Err {}) = errName t1 == errName t2 && errMsg t1 == errMsg t2
-- eqByHash t1@(Err {}) _           = False
-- eqByHash _ t2@(Err {})           = False
-- eqByHash t1 t2                   = treeHash t1 == treeHash t2 -- TODO finish writing this

-- TODO write this? consider whether it's faster for any important use case
treeEqByHash :: HashTree a -> HashTree a -> Bool
treeEqByHash = undefined

-- | For round-tripping to files, because that will change the mod times.
-- We generally want to test that everything else survives unchanged.
treeEqIgnoringModTime :: Eq a => HashTree a -> HashTree a -> Bool
treeEqIgnoringModTime t1 t2 = zeroModTime t1 == zeroModTime t2

-- TODO put this in terms of Foldable or Traversable
zeroModTime :: HashTree a -> HashTree a
zeroModTime e@(Err {}) = e
zeroModTime g@(Graft {}) = g { graftTree = zeroModTime $ graftTree g }
zeroModTime d@(Dir {}) = d
  { nodeData = (nodeData d) { modTime = ModTime 0 }
  , dirContents = map zeroModTime $ dirContents d
  }
zeroModTime t = t { nodeData = (nodeData t) { modTime = ModTime 0 } }

-- TODO once there's also a dirData, should this be BiFunctor instead?
-- TODO should this also re-hash the file, or is that not part of the fileData idea?
instance Functor HashTree where
  fmap :: (a -> b) -> HashTree a -> HashTree b
  fmap fn e@(Err  {}) = Err { errMsg = errMsg e, errName = errName e}
  fmap fn f@(File {}) = f { fileData = fn (fileData f) }
  fmap fn d@(Dir  {}) = d { dirContents = map (fmap fn) (dirContents d) }
  fmap fn l@(Link {}) = l { linkData = fmap fn (linkData l) }
  fmap fn g@(Graft {}) = g { graftTree = fmap fn (graftTree g) }

-- TODO test functor identity law

instance NFData a => NFData (HashTree a)

-- A HashTree where file dirContents are generated by QuickCheck and stored in
-- memory for round-trip tests. The ByteStrings need to be regular strict or lazy
-- ones in order to be supported here:
-- https://hackage.haskell.org/package/file-io-0.1.1/docs/System-File-OsPath.html
type TestTree = HashTree B8.ByteString

-- Given a size "budget", generate test directory dirContents
-- TODO write this using a fold with accumulator? wait, maybe no need
-- this should have sum of nNodes == size... or is it nNodes-1?
-- TODO test prop for that
arbitraryContents :: Int -> Gen [TestTree]
arbitraryContents n | n < 1 = return []
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
      arbitraryContents remNBytes >>= \cs -> return $ sortContentsByName $ recTree:cs -- TODO clean this up

sortContentsByName :: [HashTree a] -> [HashTree a]
sortContentsByName = sortBy (compare `on` treeName)

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
  !cs <- arbitraryContents $ arbsize - 1
  !mt <- arbitrary :: Gen ModTime
  !s <- return (NBytes 4096) -- TODO does dir size vary?
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
dropFileData e@(Err  {})                = Err { errName = errName e, errMsg = errMsg e }
dropFileData f@(File {})                = f {fileData = ()}
dropFileData l@(Link {})                = l {linkData = Just ()}
dropFileData g@(Graft {})               = g {graftTree = dropFileData (graftTree g) }

instance Arbitrary ProdTree where
  arbitrary :: Gen ProdTree
  arbitrary = fmap dropFileData (arbitrary :: Gen TestTree)

confirmFileHashes :: TestTree -> Bool
confirmFileHashes (File {fileData = f, nodeData=nd}) = hashBytes f == hash nd
confirmFileHashes (Dir {dirContents = cs})           = all confirmFileHashes cs
confirmFileHashes (Err {})                           = True -- TODO False?
confirmFileHashes (Graft {graftTree=t})              = confirmFileHashes t
confirmFileHashes (Link {linkData = l, nodeData=nd}) =
  case l of
    Nothing -> True
    Just ld -> hashBytes ld == hash nd

prop_confirm_file_hashes :: TestTree -> Bool
prop_confirm_file_hashes = confirmFileHashes
