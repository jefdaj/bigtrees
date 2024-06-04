{-# LANGUAGE DeriveGeneric       #-}

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
