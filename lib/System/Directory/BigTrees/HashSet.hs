{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
  , ST -- TODO is this weird to re-export?
  , Note(..)
  , emptyHashSet
  , hashSetFromTree
  , hashSetFromList
  , addTreeToHashSet
  , addNodeToHashSet
  , toSortedList
  , writeHashList
  , readHashList
  , readHashSet
  , setNote

  , hashSetDataFromLine

  , note2bs
  , s2note

  , lookupHash
  , setContainsHash

  , prop_roundtrip_HashSet_to_ByteString
  )
  where

-- TODO which of these are needed?
import Control.Monad (forM_)
import Control.Monad.ST.Strict (ST, runST)
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.Massiv.Array as A
import Data.Maybe (fromMaybe, isJust)

import Control.DeepSeq (NFData)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Data.Attoparsec.ByteString.Char8 (Parser, char, endOfLine, parseOnly, takeTill)
import Data.Attoparsec.Combinator (lookAhead, many', manyTill)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as SBS
import Data.Either
import System.Directory.BigTrees.Hash (Hash, prettyHash)
import System.Directory.BigTrees.HashLine (HashLine (..), NBytes (..), NNodes (..), hashP, joinCols,
                                           nfilesP, sizeP)
import System.Directory.BigTrees.HashTree.Base (HashTree (..), NodeData (..), ProdTree, TestTree (..),
                                           sumNodes, treeHash, treeNBytes, treeName)
import System.Directory.BigTrees.Name (Name (..), bs2op)
import qualified System.File.OsPath as SFO
import System.IO (Handle, IOMode (..))
import System.OsPath (OsPath)
import Test.QuickCheck (Arbitrary (..), Property, arbitrary)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)

import qualified System.OsString as SOS
import qualified System.OsString.Internal.Types as SOS
import qualified Data.ByteString.Short as SBS


--- types ---

-- TODO which string type would be best for use in the sets?
-- A: for this version, whatever can be done easily! String maybe via show?
-- newtype Note = Note String
--   deriving (Eq, Ord, Read, Show, Generic)
-- instance NFData Note

-- Basically the same as a Name, except technically you could put '/' in a Note
newtype Note = Note { unNote :: SOS.OsString }
  deriving (Eq, Generic, Ord, Show)

deriving instance NFData Note

-- | Hopefully this won't be too big for RAM in most cases, because we're only
-- storing hashes and one note which can be short, rather than a list of full
-- paths like in `DupeMap`s.
data SetData = SetData
  { sdNodes :: NNodes
  , sdBytes :: NBytes
  , sdNote  :: Note
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData SetData

-- | For debugging and simpler read/write. Should be readily convertable
-- to/from HashSet.
type HashList = [(Hash, SetData)]

-- TODO rename BigSet and HashTree -> BigTree?
-- TODO would the unwrapped bytestring hash be more efficient here?
type HashSet s = C.HashTable s Hash SetData


--- hash set from tree ---

emptyHashSet :: Int -> ST s (HashSet s)
emptyHashSet = H.newSized

-- TODO can this be done with other hashtrees generically, or have to drop data first?
hashSetFromTree :: ProdTree -> ST s (HashSet s)
hashSetFromTree t = do
  -- let (NNodes n) = 1000 -- sumNodes t -- TODO leak here? would force evaluation
  h <- H.newSized 1000
  addTreeToHashSet Nothing h t
  return h

hashSetFromList :: HashList -> ST s (HashSet s)
hashSetFromList ls = do
  s <- H.newSized $ length ls
  forM_ ls $ uncurry (addNodeToHashSet s)
  return s

-- addToHashSet :: HashSet s -> ProdTree -> ST s ()
-- addToHashSet h = addToHashSet' h ""

-- inserts all nodes from a tree into an existing hashset in ST s
addTreeToHashSet :: Maybe Note -> HashSet s -> ProdTree -> ST s ()
addTreeToHashSet _ _ (Err {}) = return ()
addTreeToHashSet mn s t@(Dir {}) = do
  mapM_ (addTreeToHashSet mn s) $ dirContents t
  addNodeToHashSet s (treeHash t) $ setDataFromNode mn t
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


--- hash set from .bigtree file (streaming) ---

-- TODO should link targets be used for anything here?
hashSetDataFromLine :: Maybe Note -> HashLine -> Maybe (Hash, SetData)
hashSetDataFromLine mn (ErrLine {}) = Nothing
hashSetDataFromLine mn (HashLine (_,_,h,_,nb,nn,Name n,_)) = Just (h, sd)
  where sd = SetData
               { sdNote  = fromMaybe (Note n) mn
               , sdBytes = nb
               , sdNodes = nn
               }


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
  deriving (Eq, Ord, Show, Generic)

instance NFData HashSetLine

listToLines :: HashList -> [HashSetLine]
listToLines = map elemToLine
  where
    elemToLine (h, sd) = HashSetLine (h, sdNodes sd, sdBytes sd, sdNote sd)

prettySetLine :: HashSetLine -> B8.ByteString
prettySetLine (HashSetLine (h, NNodes nn, NBytes nb, n)) = joinCols
  [ prettyHash h
  , B8.pack $ show nn
  , B8.pack $ show nb
  , note2bs n <> B8.singleton '\NUL'
  ]

note2bs :: Note -> B8.ByteString
note2bs = SBS.fromShort . SOS.getPosixString . SOS.getOsString . unNote

s2note :: String -> Note
s2note = Note . SOS.OsString . SOS.PosixString . SBS.toShort . B8.pack

serializeHashList :: HashList -> B8.ByteString
serializeHashList = B8.unlines . map prettySetLine . listToLines

hWriteHashListBody :: Handle -> HashList -> IO ()
hWriteHashListBody h l = B8.hPutStr h $ serializeHashList l

writeHashList :: OsPath -> HashList -> IO ()
writeHashList path l = SFO.withFile path WriteMode $ \h -> hWriteHashListBody h l

-- hWriteTree :: [String] -> Handle -> HashTree a -> IO ()
-- hWriteTree es h tree = do
--   hWriteHeader   h es
--   hWriteTreeBody h tree
--   hWriteFooter   h


--- parse hashset from file ---




-- This doesn't require a fancy parser, but might as well do one because we
-- have most of the primitives already...

-- copied from nameP
noteP :: Parser Note
noteP = do
  -- TODO sepP here?
  bs <- takeTill (== '\NUL')
  _  <- char '\NUL'
  -- _  <- option undefined $ char '\t' -- TODO if this works, move sepP from HashLine
  return $ Note $ bs2op bs

-- TODO document valid note chars
setLineP :: Parser HashSetLine
setLineP = do
  h  <- hashP
  nn <- nfilesP
  nb <- sizeP
  n  <- noteP
  _  <- endOfLine
  return $ HashSetLine (h, nn, nb, n)

linesP :: Parser [HashSetLine]
linesP = many' setLineP

parseHashSetLines :: B8.ByteString -> Either String [HashSetLine]
parseHashSetLines = parseOnly linesP

parseHashList :: B8.ByteString -> Either String HashList
parseHashList bs = parseHashSetLines bs <&> map f
  where
    f (HashSetLine (h, nn, nb, n)) = (h, SetData nn nb n)

-- TODO any reason to pass on the Either rather than making it an error?
readHashList :: OsPath -> IO HashList
readHashList path = do
  eHL <- SFO.readFile' path <&> parseHashList
  case eHL of
    Left msg -> error $ "failed to read hashset: " ++ msg
    Right hl -> return hl

readHashSet :: OsPath -> IO (ST s (HashSet s))
readHashSet path = readHashList path >>= return . hashSetFromList

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


--- test whether hash is in set ---

-- TODO clean up once you understand ST better
lookupHash :: (forall s. ST s (HashSet s)) -> Hash -> Maybe SetData
lookupHash set hash = runST $ do
  s' <- set
  C.lookup s' hash

-- TODO clean up once you understand ST better
setContainsHash :: (forall s. ST s (HashSet s)) -> Hash -> Bool
setContainsHash set hash = runST $ do
  s' <- set
  mSetData <- C.lookup s' hash
  return $ isJust mSetData
