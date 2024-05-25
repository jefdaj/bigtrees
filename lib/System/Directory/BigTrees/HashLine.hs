{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Directory.BigTrees.HashLine

  ( HashLine(..)
  , TreeType(..)
  , Depth(..)
  , ModTime(..)
  , NBytes(..)
  , NNodes(..)
  , bsBytes
  -- , Hash(..) TODO re-export here? And Name too?
  , prettyLine
  , parseHashLine -- TODO remove? not actually used
  , parseHashLines
  , sepChar
  , hashLineFields
  , join

  -- for testing (TODO remove?)
  -- , nameP
  -- , lineP
  -- , linesP

  )
  where

-- TODO would be better to adapt AnchoredDirTree with a custom node type than re-implement stuff

import Control.DeepSeq (NFData (..))
import Data.Attoparsec.ByteString (skipWhile)
import Data.Attoparsec.ByteString.Char8 (Parser, anyChar, char, choice, digit, endOfInput,
                                         endOfLine, isEndOfLine, manyTill, parseOnly, sepBy', take)
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.Attoparsec.Combinator (lookAhead)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as BS
import Data.Either (fromRight)
import Data.Maybe (catMaybes)
import Prelude hiding (take)
import System.Directory.BigTrees.Hash (Hash (Hash), digestLength, prettyHash)
import System.Directory.BigTrees.Name (Name (..), breadcrumbs2fp, fp2n, n2fp)
import Test.QuickCheck (Arbitrary (..), Gen, choose, suchThat)
import TH.Derive ()
import GHC.Generics (Generic)

-----------
-- types --
-----------

-- for distinguishing beween files and dirs
data TreeType = D | F
  deriving (Eq, Ord, Read, Show)

instance NFData TreeType
  where rnf :: TreeType -> ()
        rnf = const () -- TODO is this valid?

newtype Depth
  = Depth Int
  deriving (Eq, Ord, Read, Show)

newtype ModTime = ModTime Integer
  deriving (Eq, Ord, Num, Read, Show, Generic)

instance Arbitrary ModTime where
  -- random time between 2000-01-01 and 2024-01-01
  -- (numbers don't matter much as long as they're in the past?)
  arbitrary = ModTime <$> choose (946684800, 1704067200)

instance NFData ModTime

newtype NBytes = NBytes Integer
  deriving (Eq, Ord, Num, Read, Show, Generic)

-- TODO Arbitrary NBytes instance?
--      so far I'm just generating integers as appropriate elsewhere

instance NFData NBytes

-- Conveniently, the size in bytes of a ByteString equals its length
bsBytes :: B8.ByteString -> NBytes
bsBytes = NBytes . toInteger . B8.length

-- TODO does this NFData instance work? if not, use separate clause like the others
-- TODO call it NNodes for accuracy? ppl will understand NNodes better
-- TODO Integer? think about whether it'll impact DupeMap negatively
newtype NNodes = NNodes Int
  deriving (Eq, Ord, Num, Read, Show, Generic, NFData)

-- TODO make a skip type here, or in hashtree?
-- TODO remove the tuple part now?
newtype HashLine
  = HashLine (TreeType, Depth, Hash, ModTime, NBytes, NNodes, Name)
  deriving (Eq, Ord, Read, Show)

---------------
-- instances --
---------------

-- TODO remove?
instance Arbitrary Depth where

  arbitrary :: Gen Depth
  arbitrary = Depth <$> ((arbitrary :: Gen Int) `suchThat` (>= 0))

  shrink :: Depth -> [Depth]
  shrink _ = []

instance Arbitrary TreeType where

  --  TODO oneof
  arbitrary :: Gen TreeType
  arbitrary = do
    n <- choose (0,1 :: Int)
    return $ [F, D] !! n

  -- you could shrink D -> F, but not without changing the rest of the hashline
  shrink :: TreeType -> [TreeType]
  shrink _ = []

-- TODO can you really have an arbitrary hashline without the rest of a tree?
-- TODO remove?
instance Arbitrary HashLine where

  arbitrary :: Gen HashLine
  arbitrary = do
    tt <- arbitrary :: Gen TreeType
    il <- arbitrary :: Gen Depth
    h  <- arbitrary :: Gen Hash
    mt <- arbitrary :: Gen ModTime
    s  <- fmap NBytes $ choose (0, 10000) -- TODO does it matter?
    f  <- case tt of
            D -> fmap NNodes $ choose (0, 10000) -- TODO does it matter?
            _ -> return 1
    n  <- arbitrary :: Gen Name
    return $ HashLine (tt, il, h, mt, s, f, n)

  -- only shrinks the filename
  -- TODO also change the treetype?
  shrink :: HashLine -> [HashLine]
  shrink (HashLine (tt, il, h, mt, s, f, n)) = map (\n' -> HashLine (tt, il, h, mt, s, f, n')) (shrink n)

-----------
-- print --
-----------

-- join ByteStrings with the separator char (currently tab)
join :: [B8.ByteString] -> B8.ByteString
join = B8.intercalate $ B8.singleton sepChar

-- TODO use this more directly?
-- For now it's only imported by HeadFoot to use in the Header
hashLineFields :: [String]
hashLineFields = ["type", "depth", "hash", "modtime", "size", "name"]

-- TODO actual Pretty instance
-- TODO avoid encoding as UTF-8 if possible; use actual bytestring directly
-- TODO rename/move this? it's used in printing lines and also find paths
-- TODO make this a helper and export 2 fns: prettyHashLine, prettyPathLine?
-- note: p can have weird characters, so it should be handled only as ByteString
prettyLine :: Maybe [Name] -> HashLine -> B8.ByteString
prettyLine breadcrumbs (HashLine (t, Depth n, h, ModTime mt, NBytes s, NNodes f, name)) =
  let node = case breadcrumbs of
               Nothing -> n2fp name
               Just ns -> breadcrumbs2fp $ name:ns
  in join
       -- TODO make the metadata configurable here?
       [ B8.pack $ show t
       , B8.pack $ show n
       , prettyHash h
       , B8.pack $ show mt
       , B8.pack $ show s
       , B8.pack $ show f
       , B8.pack node -- TODO n2b?
       ]

------------
-- parser --
------------

-- TODO rewrite a lot of this to deal flexibly with tables? or will order stay fixed?

sepChar :: Char
sepChar = '\t'

pSep :: Parser Char
pSep = char sepChar

typeP :: Parser TreeType
typeP = do
  t <- choice [char 'D', char 'F'] <* pSep
  return $ read [t]

hashP :: Parser Hash
hashP = do
  h <- take digestLength -- TODO any need to sanitize these?
  _ <- pSep
  return $ Hash $ BS.toShort h

{- Like endOfLine, but make sure D/F comes next followed by a valid hash digest
 - instead of the rest of a filename. This catches the rare case where a
 - filename contains a newline followed by D or F. You could still construct a
 - filename that would fool it, but it would be extremely unlikely to happen by
 - chance.
 - TODO can it use null-separated lines instead like -print0?
 -}
breakP :: Parser ()
breakP = endOfLine >> choice [typeP >> numStrP >> hashP >> return (), endOfInput]

-- TODO should anyChar be anything except forward slash and the null char?
nameP :: Parser Name
nameP = fmap fp2n $ do
  c  <- anyChar
  cs <- manyTill anyChar $ lookAhead breakP
  return (c:cs)

-- TODO is there a built-in thing for this?
numStrP :: Parser String
numStrP = manyTill digit pSep

-- TODO applicative version?
depthP :: Parser Depth
depthP = numStrP >>= return . Depth . read

-- TODO applicative version?
modTimeP :: Parser ModTime
modTimeP = numStrP >>= return . ModTime . read

-- TODO applicative version?
-- TODO rename nbytesP
sizeP :: Parser NBytes
sizeP = numStrP >>= return . NBytes . read

-- TODO applicative version?
nfilesP :: Parser NNodes
nfilesP = numStrP >>= return . NNodes . read

-- TODO is there a cleaner syntax for this?
-- TODO this should still count up total files when given a max depth
lineP :: Maybe Int -> Parser (Maybe HashLine)
lineP md = do
  t <- typeP
  (Depth i) <- depthP
  case md of
    Nothing -> parseTheRest t (Depth i)
    Just d -> do
      if i > d
        then do
          skipWhile (not . isEndOfLine)
          lookAhead breakP
          return Nothing
        else parseTheRest t (Depth i)
  where
    parseTheRest t i = do
      h <- hashP
      mt <- modTimeP
      s <- sizeP
      f <- nfilesP
      p <- nameP
      -- return $ trace ("finished: " ++ show (t, i, h, p)) $ Just (t, i, h, p)
      return $ Just (HashLine (t, i, h, mt, s, f, p))

linesP :: Maybe Int -> Parser [HashLine]
linesP md = do
  hls <- sepBy' (lineP md) endOfLine
  return $ catMaybes hls -- TODO count skipped lines here?

fileP :: Maybe Int -> Parser [HashLine]
fileP md = linesP md <* endOfLine <* endOfInput

-- TODO use bytestring the whole time rather than converting
-- TODO should this propogate the Either?
-- TODO any more elegant way to make the parsing strict?
parseHashLines :: Maybe Int -> B8.ByteString -> [HashLine]
parseHashLines md = fromRight [] . parseOnly (fileP md)

parseHashLine :: B8.ByteString -> Either String (Maybe HashLine)
parseHashLine bs = A8.parseOnly (lineP Nothing) (B8.append bs "\n")
