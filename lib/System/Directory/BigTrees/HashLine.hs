{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE QuasiQuotes                #-}

module System.Directory.BigTrees.HashLine

  ( HashLine(..)
  , TreeType(..)
  , Depth(..)
  , ModTime(..)
  , NBytes(..)
  , NNodes(..)
  , ErrMsg(..)
  -- , Hash(..) TODO re-export here? And Name too?

  , bsBytes
  , prettyLine
  , hashLineP
  , breakP
  , parseHashLine
  , sepChar
  , hashLineFields
  -- , ospTabJoin
  , hashP
  , nfilesP
  , sizeP
  , nameP
  , numStrP
  , typeP

  -- for testing (TODO remove?)
  -- , nameP
  -- , linesP
  , bench_roundtrip_HashLines_to_ByteString
  , prop_roundtrip_HashLines_to_ByteString
  , genHashLinesBS
  , parseHashLinesBS
  , joinCols

  )
  where

-- TODO would be better to adapt AnchoredDirTree with a custom node type than re-implement stuff

import Control.DeepSeq (NFData (..), force)
import Control.Monad (void)
import Data.Attoparsec.ByteString (skipWhile)
import Data.Attoparsec.ByteString.Char8 (Parser, anyChar, char, choice, digit, endOfInput,
                                         endOfLine, isEndOfLine, manyTill, parseOnly, take, takeTill)
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.Attoparsec.Combinator (lookAhead, sepBy')
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LBS
import Data.Either (fromRight, either) -- TODO ok to import only for doctest?
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Prelude hiding (take)
import System.Directory.BigTrees.Hash (Hash (Hash), digestLength, prettyHash)
import System.Directory.BigTrees.Name (Name (..), NamesRev, breadcrumbs2bs, n2bs, bs2n, nameP)
import Test.QuickCheck (Arbitrary (..), Gen, choose, suchThat, Property, resize, generate)
import TH.Derive ()
import qualified System.OsPath as OSP
-- import Data.List (intercalate)
import System.IO (utf8)
import System.OsString.Internal       -- TODO specifics
import System.OsString.Internal.Types -- TODO specifics
import qualified System.OsPath.Internal as OSPI

-----------
-- types --
-----------

-- for distinguishing beween files and dirs
-- TODO is B a good notation for broken link, or should that be LB?
-- TODO unify this with the actual constructors in HashTree.Base?
data TreeType = D | F | L | B | E
  deriving (Eq, Ord, Read, Show)

instance NFData TreeType
  where rnf :: TreeType -> ()
        rnf = const () -- TODO is this valid?

newtype Depth
  = Depth Int
  deriving (Eq, Ord, Read, Show, Generic)

instance NFData Depth

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

newtype ErrMsg = ErrMsg String
  deriving (Eq, Ord, Read, Show, Generic, Arbitrary)

instance NFData ErrMsg

-- TODO does this NFData instance work? if not, use separate clause like the others
-- TODO call it NNodes for accuracy? ppl will understand NNodes better
-- TODO Integer? think about whether it'll impact DupeMap negatively
newtype NNodes = NNodes Int
  deriving (Eq, Ord, Num, Read, Show, Generic, NFData)

-- TODO make a skip type here, or in hashtree?
-- TODO remove the tuple part now?
data HashLine
  = HashLine (TreeType, Depth, Hash, ModTime, NBytes, NNodes, Name)
  | ErrLine  (Depth, ErrMsg, Name)
  deriving (Eq, Ord, Show, Generic)

instance NFData HashLine

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
    n <- choose (0,2 :: Int) -- TODO make errors less common?
    return $ [E, F, D] !! n

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
    s  <- NBytes <$> choose (0, 10000) -- TODO does it matter?
    f  <- case tt of
            D -> NNodes <$> choose (0, 10000) -- TODO does it matter?
            E -> return 0 -- TODO 1? but it could be a dir with any number really
            F -> return 1
    n  <- arbitrary :: Gen Name
    return $ HashLine (tt, il, h, mt, s, f, n)

  -- only shrinks the filename
  -- TODO also change the treetype?
  shrink :: HashLine -> [HashLine]
  shrink (HashLine (tt, il, h, mt, s, f, n)) = map (\n' -> HashLine (tt, il, h, mt, s, f, n')) (shrink n)

-----------
-- print --
-----------

joinCols :: [B8.ByteString] -> B8.ByteString
joinCols = B8.intercalate (B8.singleton sepChar)

-- TODO use this more directly?
-- For now it's only imported by HeadFoot to use in the Header
hashLineFields :: [String]
hashLineFields = ["type", "depth", "hash", "modtime", "nbytes", "nfiles", "name"]

-- TODO rename to something less weird
-- TODO unify this with the equivalent for bigsets? and path lists?
-- TODO Binary/Bytable instance here instead?
-- TODO avoid encoding as UTF-8; use actual bytestring directly
-- TODO treat empty NamesRev as Nothing?
-- TODO make this a helper and export 2 fns: prettyHashLine, prettyPathLine?
-- TODO should it not be possible to have an ErrLine for bigsets and/or find path lists?
-- note: p can have weird characters, so it should be handled only as ByteString
prettyLine :: Maybe NamesRev -> HashLine -> B8.ByteString

prettyLine breadcrumbs (ErrLine (Depth d, ErrMsg m, name)) =
  let node = case breadcrumbs of
               Nothing -> n2bs name
               Just ns -> breadcrumbs2bs $ name:ns
  in joinCols
       [ B8.pack $ show E
       , B8.pack $ show d
       , B8.pack $ show m -- unlike other hashline components, this should be quoted
       , node <> B8.singleton '\NUL'
       ]

prettyLine breadcrumbs (HashLine (t, Depth n, h, ModTime mt, NBytes s, NNodes f, name)) =
  let node = case breadcrumbs of
               Nothing -> n2bs name
               Just ns -> breadcrumbs2bs $ name:ns
  in joinCols
       [ B8.pack $ show t
       , B8.pack $ show n
       , prettyHash h
       , B8.pack $ show mt
       , B8.pack $ show s
       , B8.pack $ show f
       , node <> B8.singleton '\NUL'
       ]

-- TODO do this without IO?
genHashLinesBS :: Int -> IO B8.ByteString
genHashLinesBS n = do
  -- TODO is this resizing the lines themselves in addition to the list?
  (ls :: [HashLine]) <- generate $ resize n arbitrary
  let bs = force $ B8.unlines $ map (prettyLine Nothing) ls
  return bs

-- TODO rename? confusingly sounds "bigger" than breakP
nullBreakP :: Parser ()
nullBreakP = char '\NUL' *> endOfLine

-- This returns the length of the list, which can either be throw out or used
-- to double-check that all the HashLines parsed correctly.
--
-- Manual usage:
--
-- >>> ls <- genHashLinesBS 20
-- >>> hls = parseHashLinesBS ls
-- >>> either show (const "test passed") hls
-- >>> "test passed"
--
parseHashLinesBS :: B8.ByteString -> Either String [HashLine]
parseHashLinesBS bs = 
  (force . catMaybes) <$>
  parseOnly (sepBy' (hashLineP Nothing) nullBreakP) bs

-- Note that these random lines can't be parsed into a valid tree;
-- the only test the HashLine parser
bench_roundtrip_HashLines_to_ByteString :: Int -> IO Bool
bench_roundtrip_HashLines_to_ByteString n = do
  bs <- genHashLinesBS n
  case parseHashLinesBS bs of
    Left msg -> error msg
    Right ls -> return $ length ls == n

-- Note that these lines won't form a valid tree.
prop_roundtrip_HashLines_to_ByteString :: [HashLine] -> Bool
prop_roundtrip_HashLines_to_ByteString hls =
  let bs  = B8.unlines $ map (prettyLine Nothing) hls
      res = parseOnly (sepBy' (hashLineP Nothing) nullBreakP) bs
  in case fmap catMaybes res of
       Left _ -> False
       Right hls' -> hls' == hls


------------
-- parser --
------------

-- TODO rewrite a lot of this to deal flexibly with tables? or will order stay fixed?

sepChar :: Char
sepChar = '\t'

sepP :: Parser Char
sepP = char sepChar

typeP :: Parser TreeType
typeP = do
  t <- choice [char 'D', char 'F', char 'L', char 'B', char 'E'] <* sepP
  return $ read [t]

hashP :: Parser Hash
hashP = do
  h <- take digestLength -- TODO any need to sanitize these?
  _ <- sepP
  return $ Hash $ SBS.toShort h

{- Like endOfLine, but make sure D/E/F comes next followed by a valid hash digest
 - instead of the rest of a filename. This catches the rare case where a
 - filename contains a newline followed by D or F. You could still construct a
 - filename that would fool it, but it would be extremely unlikely to happen by
 - chance.
 - TODO can it use null-separated lines instead like -print0?
 -}
breakP :: Parser ()
breakP =
  nullBreakP >>
  choice
    [ void (char '#'), typeP >> numStrP >> return ()
    , endOfInput
    ]

-- TODO is there a built-in thing for this?
numStrP :: Parser String
numStrP = manyTill digit sepP

-- TODO applicative version?
depthP :: Parser Depth
depthP = numStrP <&> (Depth . read)

-- TODO applicative version?
modTimeP :: Parser ModTime
modTimeP = numStrP <&> (ModTime . read)

-- TODO applicative version?
-- TODO rename nbytesP
sizeP :: Parser NBytes
sizeP = numStrP <&> (NBytes . read)

-- TODO applicative version?
nfilesP :: Parser NNodes
nfilesP = numStrP <&> (NNodes . read)

-- TODO is there a cleaner syntax for this?
-- TODO this should still count up total files when given a max depth
hashLineP :: Maybe Int -> Parser (Maybe HashLine)
hashLineP md = do
  !t <- typeP
  !(Depth i) <- depthP
  case md of
    Nothing -> Just <$> parseTheRest t (Depth i)
    Just d -> do
      if i > d
        then do
          skipWhile (not . isEndOfLine)
          lookAhead breakP
          return Nothing
        else Just <$> parseTheRest t (Depth i)

quoteChar :: Char
quoteChar = '"'

-- TODO there should be something built-in for this, right?
quoteP :: Parser Char
quoteP = char quoteChar

errP :: Parser ErrMsg
errP = do
  !_ <- quoteP
  !msg <- manyTill anyChar quoteP
  -- _ <- quoteP -- TODO does msg already parse this?
  return $ ErrMsg msg

parseTheRest :: TreeType -> Depth -> Parser HashLine

parseTheRest E i = do
  !m <- errP
  !n <- nameP
  -- TODO does this have to be done here when the next line is a comment?
  -- _ <- char '\NUL'
  return $ ErrLine (i, m, n)

-- this works on F or D; only E is different so far
parseTheRest t i = do
  !h <- hashP
  !mt <- modTimeP
  !s <- sizeP
  !f <- nfilesP
  !p <- nameP
  -- TODO does this have to be done here when the next line is a comment?
  -- _ <- char '\NUL'
  -- return $ trace ("finished: " ++ show (t, i, h, p)) $ Just (t, i, h, p)
  return $ HashLine (t, i, h, mt, s, f, p)

-- TODO proper eitherToMaybe or similar idiom
-- TODO use String here?
parseHashLine :: B8.ByteString -> Maybe HashLine
parseHashLine bs = case A8.parseOnly (hashLineP Nothing) (B8.append bs "\n") of
  Left _         -> Nothing
  Right Nothing  -> Nothing
  Right (Just x) -> Just x

