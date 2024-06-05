{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-|
Similar in structure to `DupeMap`, but a `HashSet` doesn't care about paths or
copy numbers. It's meant as a compact on-disk store of a set of hashes for a
user-defined purpose, simpler and smaller than a `HashTree`.

Because it has to be deduplicated, there's also no point trying to stream
creating a HashSet without holding it all in memory. Therefore reading +
writing it is simpler. It can even be kept in sorted order.

Example use cases:

* Hashes that have already been trashed and can be trashed when seen again
* Hashes of lost files to scan for and retreive
* Hashes of all parts of a tree in one location that are over 10M or 100 files,
  to detect dupes in another location.

The "note" field defaults to the `Name` of the tree node, but can be overridden
in batches from the command line. For example you might have a huge set of
hashes simply labeled "backed up" or "delete".

The `NBytes` and `NNodes` fields are for filtering the set to make it smaller.
-}

module System.Directory.BigTrees.HashSet
  ( SetData(..)
  , HashList
  , HashSet
  , Note(..)
  -- , emptyHashSet
  , hashSetFromTree
  , hashSetFromList
  , addTreeToHashSet
  , toSortedList
  , writeHashList
  , readHashList
  , setNote

  , prop_roundtrip_HashSet_to_ByteString
  )
  where

-- TODO which of these are needed?
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.Massiv.Array as A
import Control.Monad.ST.Strict (ST, runST)
import Control.Monad (forM)
import Data.Maybe (fromMaybe)

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import qualified Data.Text as T

import System.Directory.BigTrees.Name (Name(..))
import System.Directory.BigTrees.Hash (Hash)
import System.Directory.BigTrees.HashLine (NBytes(..), NNodes (..), join, hashP, nfilesP, sizeP)
import System.Directory.BigTrees.HashTree (HashTree (..), TestTree(..), NodeData (..), ProdTree, sumNodes, treeName, treeHash, treeNBytes)
import qualified Data.ByteString.Char8 as B8
import System.Directory.BigTrees.Hash (Hash, prettyHash)
import System.IO (Handle, IOMode(..), withFile) -- , IOMode (..), hFlush, stdout, withFile)
import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly, anyChar, endOfLine)
import Data.Attoparsec.Combinator (lookAhead, manyTill, sepBy')
import Data.Either -- TODO remove? or specifics
import Test.QuickCheck (Arbitrary (..), Property, arbitrary)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)


--- types ---

-- TODO which string type would be best for use in the sets?
newtype Note = Note T.Text
  deriving (Eq, Ord, Read, Show, Generic)

instance NFData Note

-- | Hopefully this won't be too big for RAM in most cases, because we're only
-- storing hashes and one note which can be short, rather than a list of full
-- paths like in `DupeMap`s.
data SetData = SetData
  { sdNodes :: NNodes
  , sdBytes :: NBytes
  , sdNote  :: Note
  } 
  deriving (Eq, Ord, Read, Show, Generic)

instance NFData SetData

-- | For debugging and simpler read/write. Should be readily convertable
-- to/from HashSet.
type HashList = [(Hash, SetData)]

-- TODO rename BigSet and HashTree -> BigTree?
-- TODO would the unwrapped bytestring hash be more efficient here?
type HashSet s = C.HashTable s Hash SetData


--- create hash sets ---

-- TODO remove? not sure if useful
-- emptyHashSet :: ST s (HashSet s)
-- emptyHashSet = H.newSized 1

-- TODO can this be done with other hashtrees generically, or have to drop data first?
hashSetFromTree :: ProdTree -> ST s (HashSet s)
hashSetFromTree t = do
  let (NNodes n) = sumNodes t
  h <- H.newSized n
  addTreeToHashSet Nothing h t
  return h

hashSetFromList :: HashList -> ST s (HashSet s)
hashSetFromList ls = do
  s <- H.newSized $ length ls
  forM ls $ \(h, sd) -> addNodeToHashSet s h sd
  return s

-- addToHashSet :: HashSet s -> ProdTree -> ST s ()
-- addToHashSet h = addToHashSet' h ""

-- inserts all nodes from a tree into an existing hashset in ST s
addTreeToHashSet :: Maybe Note -> HashSet s -> ProdTree -> ST s ()
addTreeToHashSet _ _ (Err {}) = return ()
addTreeToHashSet mn s t@(Dir {}) = do
  addNodeToHashSet s (treeHash t) $ setDataFromNode mn t
  mapM_ (addTreeToHashSet mn s) $ dirContents t
addTreeToHashSet mn s t =
  addNodeToHashSet s (treeHash t) $ setDataFromNode mn t

setDataFromNode :: Maybe Note -> HashTree () -> SetData
setDataFromNode mn tree =
  let (Name n) = treeName tree
  in SetData
       { sdNote  = fromMaybe (Note n) mn
       , sdBytes = treeNBytes tree
       , sdNodes = sumNodes tree
       }

-- inserts one node into an existing dupemap in ST s,
-- overwriting the existing note if any (all other fields should be the same)
-- TODO assert that other fields are the same? prop test for that
-- TODO should this return the set?
addNodeToHashSet :: HashSet s -> Hash -> SetData -> ST s ()
addNodeToHashSet s h sd = H.insert s h sd


--- quicksort hashset to list ---

-- Some of this is cargo-culted from my own DupeMap code,
-- and could use some optimization/cutting down here.

type HashSetVec = A.Array A.BN A.Ix1 (Hash, SetData)

toSortedList :: (forall s. ST s (HashSet s)) -> HashList
toSortedList hs = sortedL
  where
    unsortedL = runST $ toUnsortedList =<< hs
    unsorted = A.fromList A.Par unsortedL :: HashSetVec
    sorted   = A.quicksort $ A.compute unsorted :: HashSetVec
    sortedL  = A.toList sorted

-- | Not-for-export helper
-- TODO any need to adjust the hash for different scoring here?
-- Basically just want alphabetical, which should be the default.
toUnsortedList :: HashSet s -> ST s HashList
toUnsortedList = H.foldM (\hs h -> return (h:hs)) []
  -- where
    -- score (Hash h, _) = h -- alphabetical, right?


--- write hashset to file ---

data HashSetLine
  = HashSetLine (Hash, NNodes, NBytes, Note)
  deriving (Eq, Ord, Read, Show, Generic)

instance NFData HashSetLine

listToLines :: HashList -> [HashSetLine]
listToLines = map elemToLine
  where
    elemToLine (h, sd) = HashSetLine (h, sdNodes sd, sdBytes sd, sdNote sd)

prettySetLine :: HashSetLine -> B8.ByteString
prettySetLine (HashSetLine (h, NNodes nn, NBytes nb, Note n)) = join
  [ prettyHash h
  , B8.pack $ show nn
  , B8.pack $ show nb
  , B8.pack $ T.unpack n -- TODO that can't be the best way, can it?
  ]

serializeHashList :: HashList -> B8.ByteString
serializeHashList = B8.unlines . map prettySetLine . listToLines

hWriteHashListBody :: Handle -> HashList -> IO ()
hWriteHashListBody h l = B8.hPutStr h $ serializeHashList l

writeHashList :: FilePath -> HashList -> IO ()
writeHashList path l = withFile path WriteMode $ \h -> hWriteHashListBody h l

-- hWriteTree :: [String] -> Handle -> HashTree a -> IO ()
-- hWriteTree es h tree = do
--   hWriteHeader   h es
--   hWriteTreeBody h tree
--   hWriteFooter   h


--- parse hashset from file ---

-- This doesn't require a fancy parser, but might as well do one because we
-- have most of the primitives already...

noteP :: Parser Note
noteP = do
  c  <- anyChar
  cs <- manyTill anyChar $ lookAhead endOfLine
  return $ Note $ T.pack $ c:cs

-- TODO document valid note chars
setLineP :: Parser HashSetLine
setLineP = do
  h  <- hashP
  nn <- nfilesP
  nb <- sizeP
  n  <- noteP -- TODO allow slashes in notes? newlines?
  return $ HashSetLine (h, nn, nb, n)

linesP :: Parser [HashSetLine]
linesP = sepBy' setLineP endOfLine <* endOfLine

parseHashSetLines :: B8.ByteString -> Either String [HashSetLine]
parseHashSetLines = parseOnly linesP

parseHashList :: B8.ByteString -> Either String HashList
parseHashList bs = parseHashSetLines bs >>= return . map f
  where
    f (HashSetLine (h, nn, nb, n)) = (h, SetData nn nb n)

-- TODO throw IO error rather than Left here?
readHashList :: FilePath -> IO (Either String HashList)
readHashList path = B8.readFile path >>= return . parseHashList


--- round-trip tests ---

roundtripHashListToByteString :: HashList -> Either String HashList
roundtripHashListToByteString l = parseHashList $ serializeHashList l

-- TODO rewrite without monadicIO?
prop_roundtrip_HashSet_to_ByteString :: Property
prop_roundtrip_HashSet_to_ByteString = monadicIO $ do
  (t :: ProdTree) <- pick arbitrary
  let l1  = toSortedList $ hashSetFromTree t
      eL2 = roundtripHashListToByteString l1
  assert $ eL2 == Right l1


--- set notes ---

setNote :: Note -> HashList -> HashList
setNote note = map (\(h, sd) -> (h, sd { sdNote = note }))
