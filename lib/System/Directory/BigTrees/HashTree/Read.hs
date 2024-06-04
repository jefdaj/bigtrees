{-# LANGUAGE OverloadedStrings #-}

module System.Directory.BigTrees.HashTree.Read where

-- import Control.Exception.Safe (catchAny)
import qualified Data.ByteString.Char8 as B8
import Data.List (partition)
import System.Directory.BigTrees.HashLine (Depth (..), ErrMsg (..), HashLine (..), TreeType (..),
                                           hashLineP, breakP)
import System.Directory.BigTrees.HashTree.Base (HashTree (..), NodeData (..), ProdTree, TestTree,
                                                sumNodes)
import System.Directory.BigTrees.HashTree.Build (buildTree)
import System.Directory.BigTrees.Name (Name (..))
-- import System.FilePath.Glob (Pattern)
import Data.Attoparsec.ByteString (skipWhile)
import Data.Attoparsec.ByteString.Char8 (Parser, anyChar, char, choice, digit, endOfInput,
                                         endOfLine, isEndOfLine, manyTill, parseOnly, sepBy', take)
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.Attoparsec.Combinator (lookAhead)
import Data.Either (fromRight)
import Data.Maybe (catMaybes)
import System.Directory.BigTrees.HeadFoot (Header(..), Footer(..))
import Data.Aeson (FromJSON, ToJSON, decode)

readTree :: Maybe Int -> FilePath -> IO ProdTree
readTree md path = deserializeTree md <$> B8.readFile path

-- TODO error on null string/lines?
-- TODO wtf why is reverse needed? remove that to save RAM
-- TODO refactor so there's a proper buildTree function and this uses it
-- TODO what about files with newlines in them? might need to split at \n(file|dir)
-- TODO also return header + footer?
deserializeTree :: Maybe Int -> B8.ByteString -> ProdTree
-- deserializeTree md = snd . head . foldr accTrees [] . reverse . parseHashLines md
deserializeTree md bs = case parseTreeFile md bs of
  Left msg -> error msg -- TODO Err tree here?
  Right (h, ls, f) -> case foldr accTrees [] $ reverse ls of
    []            -> Err { errName = Name "deserializeTree", errMsg = ErrMsg "no HashLines parsed" } -- TODO better name?
    ((_, tree):_) -> tree

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

-- TODO verify nfiles here, or just ignore it? should always be recalculated anyway
-- TODO use a more efficient list append type? or reverse order?
accTrees :: HashLine -> [(Depth, ProdTree)] -> [(Depth, ProdTree)]

accTrees (ErrLine (d, m, n)) cs = cs ++ [(d, Err { errMsg = m, errName = n })]

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
  B -> let l = Link
                 { linkData = Nothing -- TODO is this meaningully different from L?
                 , nodeData = NodeData
                   { name = p
                   , hash = h
                   , modTime = mt
                   , nBytes = s
                   }
                 }
       in cs ++ [(Depth i, l)]
  L -> let l = Link
                 { linkData = Just ()
                 , nodeData = NodeData
                   { name = p
                   , hash = h
                   , modTime = mt
                   , nBytes = s
                   }
                 }
       in cs ++ [(Depth i, l)]
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

--- attoparsec parsers ---

-- TODO use bytestring the whole time rather than converting
-- TODO should this propogate the Either?
-- TODO any more elegant way to make the parsing strict?
-- TODO parse rather than parseOnly?
parseTreeFile :: Maybe Int -> B8.ByteString -> Either String (Header, [HashLine], Footer)
parseTreeFile md = parseOnly (fileP md) -- TODO fix this!

linesP :: Maybe Int -> Parser [HashLine]
linesP md = do
  hls <- sepBy' (hashLineP md) endOfLine <* endOfLine
  return $ catMaybes hls -- TODO count skipped lines here?

-- bodyP :: Maybe Int -> Parser [HashLine]
-- bodyP md = linesP md -- <* endOfLine -- <* (lookAhead $ char '#')

fileP :: Maybe Int -> Parser (Header, [HashLine], Footer)
fileP md = do
  h <- headerP
  b <- linesP md
  f <- footerP
  -- _ <- endOfInput
  return (h, b, f)

commentLineP = do
  _ <- char '#'
  manyTill anyChar $ lookAhead endOfLine

headerP = do
  headerLines <- sepBy' commentLineP endOfLine <* endOfLine
  case parseHeader headerLines of
    Nothing -> fail "failed to parse header"
    Just h -> return h

footerP = do
  footerLines <- sepBy' commentLineP endOfLine -- <* endOfLine
  case parseFooter footerLines of
    Nothing -> fail "failed to parse footer"
    Just h -> return h

-- The main Attoparsec parser(s) can separate the commented section,
-- then the uncommented JSON is handled here.
-- TODO is it an Either?
parseFooter :: [String] -> Maybe Footer
parseFooter = decode . B8.fromStrict . B8.pack . unlines

-- Header is the same, except we have to lob off the final header line
-- TODO also confirm it looks as expected? tree format should be enough tho
parseHeader :: [String] -> Maybe Header
parseHeader s = case s of
  [ ] -> Nothing -- should never happen, right?
  [l] -> Nothing -- should never happen, right?
  ls  -> decode $ B8.fromStrict $ B8.pack $ unlines $ init ls
