{-# LANGUAGE DeriveGeneric       #-}

{-|
Similar in structure to `DupeMap`, but a `HashSet` doesn't care about paths or
copy numbers. It's meant as a compact on-disk store of a set of hashes for a
user-defined purpose, simpler and smaller than a `HashTree`. Example use cases:

* Hashes that have already been trashed and can be trashed when seen again
* Hashes of lost files to scan for and retreive
* Hashes of all parts of a tree in one location that are over 10M or 100 files,
  to detect dupes in another location.

The "note" field defaults to the `Name` of the tree node, but can be overridden
in batches from the command line. For example you might have a huge set of
hashes simply labeled "backed up" or "delete".

The `NBytes` and `NNodes` fields are for filtering the set to make it smaller.
-}

module System.Directory.BigTrees.HashSet where

-- TODO which of these are needed?
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Cuckoo as C
import Control.Monad.ST (ST, runST)

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import qualified Data.Text as T

import System.Directory.BigTrees.Name (Name(..))
import System.Directory.BigTrees.Hash (Hash)
import System.Directory.BigTrees.HashLine (NBytes(..), NNodes (..))
import System.Directory.BigTrees.HashTree (HashTree (..), NodeData (..), ProdTree, sumNodes, treeName, treeHash, treeNBytes)

-- | Hopefully this won't be too big for RAM in most cases, because we're only
-- storing hashes and one note which can be short, rather than a list of full
-- paths like in `DupeMap`s.
data SetData = SetData
  { sdNote  :: T.Text -- TODO which string type would be best here?
  , sdBytes :: NBytes
  , sdNodes :: NNodes
  } 
  deriving (Eq, Ord, Read, Show, Generic)

instance NFData SetData

-- TODO rename BigSet and HashTree -> BigTree?
-- TODO would the unwrapped bytestring hash be more efficient here?
type HashSet s = C.HashTable s Hash SetData

-- TODO can this be done with other hashtrees generically, or have to drop data first?
hashSetFromTree :: ProdTree -> ST s (HashSet s)
hashSetFromTree t = do
  h <- H.newSized 1
  addTreeToHashSet h t
  return h

-- addToHashSet :: HashSet s -> ProdTree -> ST s ()
-- addToHashSet h = addToHashSet' h ""

-- inserts all nodes from a tree into an existing hashset in ST s
addTreeToHashSet :: HashSet s -> ProdTree -> ST s ()
addTreeToHashSet _ (Err {}) = return ()
addTreeToHashSet s tree =
  let (Name n) = treeName tree
      sd = SetData
             { sdNote  = n
             , sdBytes = treeNBytes tree
             , sdNodes = sumNodes tree
             }
  in addNodeToHashSet s (treeHash tree) sd

-- inserts one node into an existing dupemap in ST s
-- TODO should a new name override an old one? currently old is kept
-- TODO should this return the set?
addNodeToHashSet :: HashSet s -> Hash -> SetData -> ST s ()
addNodeToHashSet s h sd = do
  existing <- H.lookup s h
  case existing of
    Nothing -> H.insert s h sd
    Just _  -> return ()
