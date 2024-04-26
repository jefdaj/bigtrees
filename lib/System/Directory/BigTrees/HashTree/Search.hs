module System.Directory.BigTrees.HashTree.Search where

import System.Directory.BigTrees.Hash
import Control.Monad (msum, when)
import System.Directory.BigTrees.HashTree.Base
import Control.DeepSeq (force)
import Control.Exception.Safe (catchAny)
import Control.Monad (msum, when)
import qualified Control.Monad.Parallel as P
import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as BS
import Data.Char (toLower)
import Data.Either (fromRight)
import Data.Function (on)
import Data.List (delete, find, nubBy, partition, sort, sortBy)
import Data.Maybe (isJust)
import Data.Store (decodeIO, encode)
import qualified System.Directory as SD
import System.Directory.BigTrees.FilePath (fp2n, n2fp, pathComponents)
import System.Directory.BigTrees.Hash (Hash, hashBytes)
import System.Directory.BigTrees.HashLine (HashLine (..), IndentLevel (IndentLevel),
                                           TreeType (D, F), lineP, prettyHashLine)
import System.Directory.BigTrees.Name (Name (..))
import qualified System.Directory.Tree as DT
import System.FilePath (joinPath, splitPath, (</>))
import System.FilePath.Glob (MatchOptions (..), Pattern, matchWith)
import System.Info (os)
import System.IO (IOMode (..), hClose, hFlush, stdout, withFile)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import Test.QuickCheck (Arbitrary (..), Gen, Property, choose, resize)
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)

import System.Directory.BigTrees.HashTree.Base (HashTree (Dir, File, contents, fileData, hash, nFiles, name),
                                                ProdTree, TestTree, countFiles, hashContents)
import System.Directory.BigTrees.HashTree.Build (buildProdTree, buildTree)


-------------------
-- search a tree --
-------------------

-- treeContainsPath :: HashTree -> FilePath -> Bool
-- treeContainsPath (File f1 _     ) f2 = f1 == f2
-- treeContainsPath (Dir  f1 _ cs _) f2
--   | f1 == f2 = True
--   | length (pathComponents f2) < 2 = False
--   | otherwise = let n   = head $ pathComponents f2
--                     f2' = joinPath $ tail $ pathComponents f2
--                 in if f1 /= n
--                   then False
--                   else any (\c -> treeContainsPath c f2') cs

treeContainsPath :: ProdTree -> FilePath -> Bool
treeContainsPath tree path = isJust $ dropTo tree path

dropTo :: ProdTree -> FilePath -> Maybe ProdTree
dropTo t@(File f1 _ ()  ) f2 = if n2fp f1 == f2 then Just t else Nothing
dropTo t@(Dir  f1 _ cs _) f2
  | n2fp f1 == f2 = Just t
  | length (pathComponents f2) < 2 = Nothing
  | otherwise = let n   = fp2n $ head $ pathComponents f2
                    f2' = joinPath $ tail $ pathComponents f2
                in if f1 /= n
                  then Nothing
                  else msum $ map (`dropTo` f2') cs

treeContainsHash :: ProdTree -> Hash -> Bool
treeContainsHash (File _ h1 ()  ) h2 = h1 == h2
treeContainsHash (Dir  _ h1 cs _) h2
  | h1 == h2 = True
  | otherwise = any (`treeContainsHash` h2) cs

-- TODO if tree contains path, be able to extract it! need for rm


