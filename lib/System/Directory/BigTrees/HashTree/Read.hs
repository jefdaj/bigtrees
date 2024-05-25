module System.Directory.BigTrees.HashTree.Read where

-- import Control.Exception.Safe (catchAny)
import qualified Data.ByteString.Char8 as B8
import Data.List (partition)
import System.Directory.BigTrees.HashLine (HashLine (..), Depth (..), TreeType (D, F),
                                           parseHashLines)
import System.Directory.BigTrees.HashTree.Base (HashTree (..), NodeData(..), ProdTree, TestTree,
                                                sumNodes)
import System.Directory.BigTrees.HashTree.Build (buildTree)
-- import System.FilePath.Glob (Pattern)

readTree :: Maybe Int -> FilePath -> IO ProdTree
readTree md path = deserializeTree md <$> B8.readFile path

-- TODO error on null string/lines?
-- TODO wtf why is reverse needed? remove that to save RAM
-- TODO refactor so there's a proper buildTree function and this uses it
-- TODO what about files with newlines in them? might need to split at \n(file|dir)
deserializeTree :: Maybe Int -> B8.ByteString -> ProdTree
deserializeTree md = snd . head . foldr accTrees [] . reverse . parseHashLines md

{- This one is confusing! It accumulates a list of trees and their depth,
 - and when it comes across a dir it uses the depths to determine
 - which files are children to put inside it vs which are siblings.
 -
 - If a value for d (max depth) is given, any line with a depth above that
 - will be dropped from the list to decrease memory usage.
 -}
-- accTrees :: Maybe Int -> HashLine -> [(Int, HashTree)] -> [(Int, HashTree)]
-- accTrees Nothing hl cs = accTrees' hl cs
-- accTrees (Just d) hl@(_, depth, _, _) cs
--   | depth > d = cs
--   | otherwise  = accTrees' hl cs

-- TODO verify that nfiles == 1 for files? or just ignore?
accTrees :: HashLine -> [(Depth, ProdTree)] -> [(Depth, ProdTree)]
accTrees (HashLine (t, Depth i, h, mt, s, _, p)) cs = case t of
  F -> let f = File
                 { fileData = ()
                 , nodeData = NodeData
                   { name = p
                   , hash = h
                   , modTime = mt
                   , nBytes = s
                   }
                 }
       in cs ++ [(Depth i, f)]
  D -> let (children, siblings) = partition (\(Depth i2, _) -> i2 > i) cs
           dir = Dir
                   { dirContents = map snd children
                   , nNodes = (sum $ map (sumNodes . snd) children)
                   , nodeData = NodeData
                     { name = p
                     , hash = h
                     , modTime = mt
                     , nBytes = s
                     }
                   }
       in siblings ++ [(Depth i, dir)]

readTestTree :: Maybe Int -> Bool -> [String] -> FilePath -> IO TestTree
readTestTree md = buildTree B8.readFile
