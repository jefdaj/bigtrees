{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module System.Directory.BigTrees.HashTree.Read where

import Control.DeepSeq (deepseq)
-- import Control.Exception.Safe (catchAny)
import qualified Data.ByteString.Char8 as B8
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (partition, sortBy)
import System.Directory.BigTrees.HashLine (Depth (..), ErrMsg (..), HashLine (..), NNodes (..),
                                           TreeType (..), hashLineP, nullBreakP, parseHashLine, linesP)
import System.Directory.BigTrees.HashTree.Base (HashTree (..), NodeData (..), ProdTree, TestTree,
                                                sumNodes, treeName)
import System.Directory.BigTrees.HashTree.Build (buildTree)
import System.Directory.BigTrees.Name (Name (..))
import System.Directory.BigTrees.Util (hTakePrevUntil)
-- import System.FilePath.Glob (Pattern)
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.Attoparsec.ByteString (skipWhile)
import Data.Attoparsec.ByteString.Char8 (Parser, anyChar, char, choice, digit, endOfInput,
                                         endOfLine, isEndOfLine, manyTill, parseOnly, sepBy', take)
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.Either (fromRight)
import Data.Maybe (catMaybes, fromJust)
import qualified System.File.OsPath as SFO
import System.IO (Handle, IOMode (..), hGetLine)
import System.OsPath (OsPath)
import System.OsString (osstr)
import System.Directory.BigTrees.HeadFoot (Header, Footer, headerP, footerP, parseFooter, commentLineP)



--- read summary info from the end of the file ---

-- TODO move to HeadFoot? HashTree.Read?
-- TODO factor out/document the max char thing
readLastHashLineAndFooter :: OsPath -> IO (Maybe (HashLine, Footer))
readLastHashLineAndFooter path = do
  mTxt <- SFO.withBinaryFile path ReadMode $ hTakePrevUntil isDepthZeroLine 10000
  case mTxt of
    Nothing -> return Nothing
    Just txt -> case filter (not . null) $ lines txt of
      [] -> return Nothing
      [_] -> return Nothing
      (l:ls) -> do
        let ml = parseHashLine $ B8.pack l
            mf = parseFooter ls
        case (ml, mf) of
          (Just l, Just f) -> return $ Just (l, f)
          _                -> return Nothing

-- Tests whether the string looks like a newline + HashLine with Depth 0
isDepthZeroLine :: String -> Bool
isDepthZeroLine ('\n':_:'\t':'0':'\t':_) = True
isDepthZeroLine _                        = False


--- read info for set-add ---

getTreeSize :: OsPath -> IO (Maybe Int)
getTreeSize path = readLastHashLineAndFooter path <&> getN
  where
    getN (Just (HashLine (_,_,_,_,_, NNodes n, _, _), _)) = Just n
    getN Nothing                                          = Nothing

-- TODO does this stream, or does it read all the lines at once?
-- TODO pass on the Left rather than throwing IO error here?
readTreeLines :: OsPath -> IO [HashLine]
readTreeLines path = do
  bs <- SFO.readFile' path
  let eSL = parseOnly (headerP *> linesP Nothing) bs
  case eSL of
    Left msg -> error $ "failed to parse " ++ show path ++ ": " ++ show msg
    Right ls -> return ls


--- read the main tree ---

readTree :: Maybe Int -> OsPath -> IO ProdTree
readTree md path = deserializeTree md <$> SFO.readFile' path

hReadTree :: Maybe Int -> Handle -> IO ProdTree
hReadTree md hdl = do
  bs <- B8.hGetContents hdl
  return $ deserializeTree Nothing bs

-- TODO error on null string/lines?
-- TODO wtf why is reverse needed? remove that to save RAM
-- TODO refactor so there's a proper buildTree function and this uses it
-- TODO what about files with newlines in them? might need to split at \n(file|dir)
-- TODO also return header + footer?
-- TODO should this return a *list* of trees? or is only one possible now that forests are gone?
deserializeTree :: Maybe Int -> B8.ByteString -> ProdTree
-- deserializeTree md = snd . head . foldr accTrees [] . reverse . parseHashLines md
deserializeTree md bs = case parseTreeFile md bs of
  Left msg -> Err { errName = Name [osstr|deserializeTree|], errMsg = ErrMsg msg }
  Right (h, ls, f) -> case foldr accTrees [] $ reverse ls of -- TODO leak here?
    []            -> Err { errName = Name [osstr|deserializeTree|], errMsg = ErrMsg "no HashLines parsed" } -- TODO better name?
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

accTrees (ErrLine (d, m, n)) cs = {-# SCC "Eappend" #-} (d, Err { errMsg = m, errName = n }):cs

-- TODO LinkLine too now? Damn gettin' complicated

accTrees (HashLine (t, Depth i, h, mt, s, _, p, mlt)) cs = case t of

  F -> let f = File
                 { fileData = ()
                 , nodeData = {-# SCC "FNodeData" #-} NodeData
                   { name = p
                   , hash = h
                   , modTime = mt
                   , nBytes = s
                   }
                 }
       in {-# SCC "Fappend" #-} (Depth i, f):cs

  B -> let l = Link
                 { linkData = Nothing -- TODO is this meaningully different from L?
                 , linkTarget = fromJust mlt
                 , linkInTree = False
                 , nodeData = {-# SCC "BNodeData" #-} NodeData
                   { name = p
                   , hash = h
                   , modTime = mt
                   , nBytes = s
                   }
                 }
       in {-# SCC "Bappend" #-} (Depth i, l):cs

  L -> let l = Link
                 { linkData = Just ()
                 , linkTarget = fromJust mlt
                 , linkInTree = True
                 , nodeData = {-# SCC "LNodeData" #-} NodeData
                   { name = p
                   , hash = h
                   , modTime = mt
                   , nBytes = s
                   }
                 }
       in {-# SCC "Lappend" #-} (Depth i, l):cs

  D -> let (children, siblings) = partitionChildrenSiblings i cs
           childrenSorted = sortBy (compare `on` (treeName . snd)) children
           dir = Dir
                   { dirContents = {-# SCC "DdirContents" #-} map snd childrenSorted
                   , nNodes = {-# SCC "DnNodes" #-} (sum $ 1 : map (sumNodes . snd) childrenSorted)
                   , nodeData = {-# SCC "DNodeData" #-} NodeData
                     { name = p
                     , hash = h
                     , modTime = mt
                     , nBytes = s
                     }
                   }
       in {-# SCC "Dappend" #-} (Depth i, dir):siblings -- TODO sort issue here?

partitionChildrenSiblings i = partition (\(Depth i2, _) -> i2 > i)

readTestTree :: Maybe Int -> Bool -> [String] -> OsPath -> IO TestTree
readTestTree md = buildTree SFO.readFile'

--- attoparsec parsers ---

-- TODO use bytestring the whole time rather than converting
-- TODO should this propogate the Either?
-- TODO any more elegant way to make the parsing strict?
-- TODO parse rather than parseOnly?
-- TODO count skipped lines here?
parseTreeFile :: Maybe Int -> B8.ByteString -> Either String (Header, [HashLine], Footer)
parseTreeFile md = parseOnly (fileP md)

-- bodyP :: Maybe Int -> Parser [HashLine]
-- bodyP md = linesP md -- <* endOfLine -- <* (lookAhead $ char '#')

-- Without `deepseq`, the large chunk of memory needed by the parsers will be
-- kept longer than necesary. Might as well force it as soon as we have the
-- data in `NFData` structures.
-- TODO warn about using linesP separately? Or put a deepseq in there too?
fileP :: Maybe Int -> Parser (Header, [HashLine], Footer)
fileP md = do
  h <- headerP
  b <- linesP md
  f <- footerP
  let res = (h, b, f)
  -- _ <- endOfInput -- TODO put back?
  return $ deepseq res res
