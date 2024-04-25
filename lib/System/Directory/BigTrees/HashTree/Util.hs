module System.Directory.BigTrees.HashTree.Util where

import System.Directory.BigTrees.HashTree.Types
import System.Directory.BigTrees.Hash
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as BS
import Data.List (delete, find, nubBy, partition, sort, sortBy)

countFiles :: HashTree a -> Int
countFiles (File {}  )    = 1
countFiles (Dir  _ _ _ n) = n

hashContents :: [HashTree a] -> Hash
hashContents = hashBytes . B8.unlines . sort . map (BS.fromShort . unHash . hash)
