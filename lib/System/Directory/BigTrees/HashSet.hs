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

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

import System.Directory.BigTrees.Hash (Hash)
import System.Directory.BigTrees.HashLine (NBytes(..), NNodes (..))
import System.Directory.BigTrees.HashTree (HashTree (..), NodeData (..), ProdTree)

-- | Hopefully this won't be too big for RAM in most cases, because we're only
-- storing hashes and one note which can be short, rather than a list of full
-- paths like in `DupeMap`s.
data SetData = SetData
  { sdNote  :: String -- TODO which string type?
  , sdBytes :: NBytes
  , sdNodes :: NNodes
  } 
  deriving (Eq, Ord, Read, Show, Generic)

instance NFData SetData

-- TODO rename BigSet and HashTree -> BigTree?
type HashSet s = C.HashTable s Hash SetData
