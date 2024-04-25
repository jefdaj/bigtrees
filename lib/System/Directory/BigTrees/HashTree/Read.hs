module System.Directory.BigTrees.HashTree.Read where

import qualified Data.ByteString.Char8 as B8
import System.Directory.BigTrees.HashLine
import System.Directory.BigTrees.HashTree.Types
import System.Directory.BigTrees.HashTree.Util (countFiles)
import Data.List (delete, find, nubBy, partition, sort, sortBy)
import Control.Exception.Safe (catchAny)
import Data.Store (decodeIO, encode)

-- try to read as binary, and fall back to text if it fails
readTree :: Maybe Int -> FilePath -> IO ProdTree
readTree md path = catchAny
                    (B8.readFile path >>= decodeIO)
                    (\_ -> deserializeTree md <$> B8.readFile path)
--   (do
--      (hs :: [HashLine]) <- decodeIO =<< B8.readFile path
--      return $ snd $ head $ foldr accTrees [] hs)
--   (\_ -> fmap deserializeTree $ B8.readFile path)

-- TODO error on null string/lines?
-- TODO wtf why is reverse needed? remove that to save RAM
-- TODO refactor so there's a proper buildTree function and this uses it
-- TODO what about files with newlines in them? might need to split at \n(file|dir)
deserializeTree :: Maybe Int -> B8.ByteString -> ProdTree
deserializeTree md = snd . head . foldr accTrees [] . reverse . parseHashes md

{- This one is confusing! It accumulates a list of trees and their indent
 - levels, and when it comes across a dir it uses the indents to determine
 - which files are children to put inside it vs which are siblings.
 -
 - If a value for d (max depth) is given, any line with an indent above that
 - will be dropped from the list to decrease memory usage.
 -}
-- accTrees :: Maybe Int -> HashLine -> [(Int, HashTree)] -> [(Int, HashTree)]
-- accTrees Nothing hl cs = accTrees' hl cs
-- accTrees (Just d) hl@(_, indent, _, _) cs
--   | indent > d = cs
--   | otherwise  = accTrees' hl cs

accTrees :: HashLine -> [(IndentLevel, ProdTree)] -> [(IndentLevel, ProdTree)]
accTrees (HashLine (t, IndentLevel i, h, p)) cs = case t of
  F -> cs ++ [(IndentLevel i, File p h ())]
  D -> let (children, siblings) = partition (\(IndentLevel i2, _) -> i2 > i) cs
           dir = Dir p h (map snd children)
                         (sum $ map (countFiles . snd) children)
       in siblings ++ [(IndentLevel i, dir)]
