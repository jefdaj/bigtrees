{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}


module System.Directory.BigTrees.HashLine

  ( HashLine(..)
  , TreeType(..)
  , Depth(..)
  , ModTime(..)
  , NBytes(..)
  , NNodes(..)
  , ErrMsg(..)
  , LinkTarget
  -- , Hash(..) TODO re-export here? And Name too?

  , bsBytes
  , prettyLine
  , hashLineP
  , breakP
  , nullBreakP
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
  , simplifyErrMsg
  , linesP
  , parseTreeFileRev
  , hParseTreeFileRev

  -- for testing (TODO remove?)
  -- , nameP
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

-- TODO are these valid? should everything come from (plain) ByteString instead?
import Data.Attoparsec.ByteString.Char8 (Parser, anyChar, char, choice, digit, endOfInput,
                                         endOfLine, isEndOfLine, manyTill, parseOnly, take,
                                         takeTill)
import qualified Data.Attoparsec.ByteString.Char8 as A8

import Data.Attoparsec.Combinator (lookAhead, sepBy')
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Either (either, fromRight)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Prelude hiding (take)
import System.Directory.BigTrees.Hash (Hash (Hash), digestLength, prettyHash)
import System.Directory.BigTrees.Name (Name (..), NamesRev, breadcrumbs2bs, bs2n, bs2op, n2bs,
                                       nameP, op2bs, sbs2op)
import System.Directory.BigTrees.Util (getBlockSize)
import qualified System.OsPath as OSP
import Test.QuickCheck (Arbitrary (..), Gen, Property, choose, generate, resize, suchThat)
import TH.Derive ()
-- import Data.List (intercalate)
import Data.Char (isAlphaNum, isAsciiLower, isAsciiUpper, isSpace)
import Data.List (elem, intercalate, sortBy)
import Data.List.Split (splitOn)
import System.IO (utf8)
import System.OsPath (OsPath)
import qualified System.OsPath.Internal as OSPI
import System.OsString.Internal.Types

import Control.DeepSeq (deepseq)
import Control.Monad (foldM, forM, forM_)
-- import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator
import qualified Data.ByteString.Char8 as B8
-- import System.Directory.BigTrees
-- import System.Directory.BigTrees.HashTree.Read
import System.IO
import System.Posix.Files

-- import Debug.Trace
import Control.Applicative (many)
import Control.Monad (forM, replicateM)
import System.Directory.BigTrees.HashLine.Base
import System.Directory.BigTrees.HeadFoot (Footer (..), Header (..), footerP, headerP)
import qualified System.File.OsPath as SFO

-----------
-- types --
-----------

-- for distinguishing beween files and dirs
-- TODO is B a good notation for broken link, or should that be LB?
-- TODO unify this with the actual constructors in HashTree.Base?
data TreeType = D | F | L | B | E | G
  deriving (Eq, Ord, Read, Show)

instance NFData TreeType
  where rnf :: TreeType -> ()
        rnf = const () -- TODO is this valid?

newtype Depth
  = Depth Int
  deriving (Eq, Ord, Num, Read, Show, Generic)

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
  deriving (Eq, Ord, Read, Show, Generic)

instance NFData ErrMsg

-- | Hacky alternative to parsing real error messages from ErrLines for now,
-- because quoted strings turn out to be harder than expected in Attoparsec.
sanitizeErrMsg :: String -> String
sanitizeErrMsg = filter $ \c ->
  isAlphaNum c
  || isAsciiLower c
  || isAsciiUpper c
  || isSpace c
  || c `elem` ("/:()[]._-" :: String)

-- | Cut the (redundant) filepath off the beginning of most IO-related error messages.
simplifyErrMsg :: String -> String
simplifyErrMsg s = if length sSplit < 2 then s' else intercalate ": " $ tail sSplit
  where
    s' = sanitizeErrMsg s
    sSplit = splitOn ": " s'

instance Arbitrary ErrMsg where
  arbitrary = ErrMsg . simplifyErrMsg <$> (arbitrary :: Gen String)
  -- TODO shrink should already be OK, right?

-- instance Arbitrary ErrMsg where
  -- arbitrary = ErrMsg <$> (arbitrary :: Gen String)
  -- shrink (ErrMsg m) = map ErrMsg <$> shrink

-- TODO does this NFData instance work? if not, use separate clause like the others
-- TODO call it NNodes for accuracy? ppl will understand NNodes better
-- TODO Integer? think about whether it'll impact DupeMap negatively
newtype NNodes = NNodes Int
  deriving (Eq, Ord, Num, Read, Show, Generic, NFData)

-- Destination of a symlink. May or may not actually exist, be in the tree etc.
type LinkTarget = OsPath

-- TODO make a skip type here, or in hashtree?
-- TODO remove the tuple part now?
data HashLine
  = HashLine (TreeType, Depth, Hash, ModTime, NBytes, NNodes, Name, Maybe LinkTarget)
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
  -- TODO L, B
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
    e  <- arbitrary :: Gen ErrMsg
    n  <- arbitrary :: Gen Name
    mlt <- case tt of
             L -> (Just . sbs2op) <$> (arbitrary :: Gen SBS.ShortByteString) -- TODO valid constraints!
             B -> (Just . sbs2op) <$> (arbitrary :: Gen SBS.ShortByteString) -- TODO valid constraints!
             _ -> return Nothing
    return $ case tt of
      E -> ErrLine  (il, e, n)
      _ -> HashLine (tt, il, h, mt, s, f, n, mlt)

  -- only shrinks the filename
  -- TODO also change the treetype?
  -- TODO also change the link target if any
  shrink :: HashLine -> [HashLine]
  shrink (ErrLine  (il, e, n)) = map (\n' -> ErrLine  (il, e, n')) (shrink n)
  shrink (HashLine (tt, il, h, mt, s, f, n, t)) =
    map (\n' -> HashLine (tt, il, h, mt, s, f, n', t)) (shrink n)

-----------
-- print --
-----------

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

prettyLine breadcrumbs (HashLine (t, Depth n, h, ModTime mt, NBytes s, NNodes f, name, mlt)) =
  let node = case breadcrumbs of
               Nothing -> n2bs name
               Just ns -> breadcrumbs2bs $ name:ns
  in joinCols $
       [ B8.pack $ show t
       , B8.pack $ show n
       , prettyHash h
       , B8.pack $ show mt
       , B8.pack $ show s
       , B8.pack $ show f
       , node <> B8.singleton '\NUL'
       ] ++ case mlt of
              Nothing -> []
              Just lt -> [op2bs lt <> B8.singleton '\NUL']

-- TODO do this without IO?
genHashLinesBS :: Int -> IO B8.ByteString
genHashLinesBS n = do
  -- TODO is this resizing the lines themselves in addition to the list?
  (ls :: [HashLine]) <- generate $ resize n arbitrary
  let bs = force $ B8.unlines $ map (prettyLine Nothing) ls
  return bs

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
  catMaybes <$>
  parseOnly (sepBy' (hashLineP Nothing) endOfLine) bs

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
      res = parseOnly (sepBy' (hashLineP Nothing) endOfLine) bs
  in case fmap catMaybes res of
       Left _     -> False
       Right hls' -> hls' == hls


------------
-- parser --
------------

-- TODO rewrite a lot of this to deal flexibly with tables? or will order stay fixed?

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
numStrP = do
  negSign <- option [] $ (:[]) <$> char '-' -- TODO clean this up
  digits  <- manyTill digit sepP -- TODO does this also consume the sep?
  return $ negSign ++ digits

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
  (Depth i) <- depthP
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

-- https://stackoverflow.com/a/40078103
-- quotedString = do
--   -- string <- between (char '"') (char '"') (many quotedStringChar)
--   string <- char '"' *> manyTill quotedStringChar (char '"')
--   _ <- sepP
--   return string
--   where
--     thingsToQuote = ['\\', '"']
--     quotedStringChar = choice [escapedChar, normalChar]
--     escapedChar = (char '\\') *> (choice $ map char thingsToQuote)
--     normalChar x = x `notElem` thingsToQuote

errP :: Parser ErrMsg
errP = do
  !msg <- quoteP *> manyTill anyChar quoteP
  _ <- sepP
  let msg' = "\"" ++ msg ++ "\"" -- TODO must be a better way, right?
  return $ ErrMsg $ read msg'

-- TODO will return an empty OsString on failure?
linkTargetP :: Parser LinkTarget
linkTargetP = do
  -- _  <- sepP -- TODO is that right?
  bs <- takeTill (== '\NUL')
  _  <- char '\NUL'
  return $ bs2op bs

parseTheRest :: TreeType -> Depth -> Parser HashLine

parseTheRest E i = do
  !m <- errP
  !n <- nameP
  -- TODO does this have to be done here when the next line is a comment?
  return $ ErrLine (i, m, n)

-- this works on F or D; only E is different so far
parseTheRest t i = do
  !h <- hashP
  !mt <- modTimeP
  !s <- sizeP
  !f <- nfilesP
  !p <- nameP -- consumes 1st \NUL
  !mlt <- choice
            -- Unless there's a link target, the line should end after 1st NUL
            [ lookAhead endOfLine >> return Nothing
            -- If there's a link target it will have a tab before and a 2nd NUL
            , fmap Just (sepP *> linkTargetP) -- consumes 2nd \NUL
            ]
  return $ HashLine (t, i, h, mt, s, f, p, mlt)

-- TODO proper eitherToMaybe or similar idiom
-- TODO use String here?
parseHashLine :: B8.ByteString -> Maybe HashLine
parseHashLine bs = case A8.parseOnly (hashLineP Nothing) (B8.append bs "\n") of
  Left _         -> Nothing
  Right Nothing  -> Nothing
  Right (Just x) -> Just x

----------------------------
-- moved from other files --
----------------------------

linesP :: Maybe Int -> Parser [HashLine]
linesP md = do
  -- these both seem to work equally well:
  -- Normally a line is followed by end of line, unless it's also the end of a chunk.
  hls <- many (hashLineP md <* endOfLine)
  -- hls <- sepBy' (hashLineP md) endOfLine
  return $ catMaybes hls -- TODO error if any non-Justs here?

----------------------------------
-- experimental: read backwards --
----------------------------------

-- forward version in use so far:
-- parseTreeFile :: Maybe Int -> B8.ByteString -> Either String (Header, [HashLine], Footer)
-- parseTreeFile md = parseOnly (fileP md)

-- like regular breakP, except "no comments": it doesn't recognize '#'
breakPNC :: Parser ()
breakPNC = do
  _ <- option undefined nullP
  _ <- endOfLine
  _ <- choice [typeP >> numStrP >> return (), endOfInput]
  return ()

-- Return all the text before the next hashline break, which should be a
-- partial line, so it can be appended to the next chunk and properly parsed
-- there.
endofprevP :: Parser B8.ByteString
endofprevP = fmap B8.pack $ manyTill anyChar (lookAhead breakPNC) <* nullBreakP

-- create list of data chunks, backwards in order through the file
-- based on https://stackoverflow.com/a/33853796
-- but i fixed a couple bugs(?)
makeReverseChunks :: Int -> Handle -> Int -> IO [Chunk]
makeReverseChunks blksize h end
  | end == 0 = return []
  | end < 0  = error "negative file index"
  | otherwise   = do
        let start = max (end - fromIntegral blksize) 0
        hSeek h AbsoluteSeek (fromIntegral start)
        blk <- B8.hGet h blksize
        rest <- makeReverseChunks blksize h start
        -- return $ (trace ("blk " ++ show start ++ "-" ++ show end ++ ":" ++ show blk) blk) : rest
        return $ blk : rest

type EndOfPrevChunk = B8.ByteString
type Chunk          = B8.ByteString

-- TODO pass maybe max depth here
parseHashLinesFromChunk :: Parser ([HashLine], EndOfPrevChunk)
parseHashLinesFromChunk = do

  -- if this is the first chunk in the file (last in iteration),
  -- there will be a header to skip before the lines start.
  --
  -- this was working better before when it was just sepBy' commentLineP endOfLine,
  -- but i worry that might swallow any line that happens to start with '#'
  --
  _ <- option undefined headerP -- TODO undefined should be safe here, no?
  _ <- option undefined $ char '\n' -- TODO why is this needed? lexeme not being handled? :(

  -- If this is the second-to-last chunk and it happens to start in the middle of the header,
  -- the easiest thing to do is pass that to the very last chunk as part of eop.

  eop <- choice
           -- If the chunk happens to start at the beginning of a valid hashline,
           -- then we should skip the eop thing by making it empty.
           [ lookAhead (hashLineP Nothing) >> return ""
           , endofprevP
           ]

  hls <- reverse <$> linesP Nothing
  -- same with the footer, if this is the final chunk in the file (first read)
  -- _ <- option [] $ sepBy' commentLineP endOfLine
  -- _ <- option [] $ many' endOfLine

  _ <- option undefined footerP
  _ <- option undefined (many endOfLine)
  -- _ <- endOfInput

  -- TODO assert that remain is empty? (or include that in parser above)
  -- remain <- manyTill anyChar endOfInput
  -- _ <- endOfInput
  -- let tfn x = if null remain then x else trace ("remain: " ++ show remain) x
  -- return $ tfn $ (trace ("hls:" ++ show (length hls)) hls, trace ("eop:" ++ show eop) eop)

  return (hls, eop)

-- The list of lines here is only used by scanl, not inside this fn;
-- the end of prev chunk is only used inside this fn and ignored by scanl.
-- TODO come up with a better way of handling Left besides infinite recursion
strictRevChunkParse
  :: Either String ([HashLine], EndOfPrevChunk)
  -> Chunk
  -> Either String ([HashLine], EndOfPrevChunk)
strictRevChunkParse (Left m) _ = Left m
strictRevChunkParse (Right (_, eop)) prev =
  let prev' = B8.append prev $ B8.append eop "\NUL\n" -- TODO why is this needed?
      -- TODO require it to parse everything to avoid any possible "remain" bugs?
      res   = case parseOnly parseHashLinesFromChunk prev' of
                Left "not enough input" -> Right ([], "") -- TODO only allow in last position of list
                -- Left msg                -> trace ("Left " ++ show msg) (Left msg)
                x                       -> x
  in deepseq res res

-- This returns a lazy list of chunk parse results, but each one will fully evaluate
-- once accessed.
-- WARNING once it hits an error (Left), it will keep repeating that error indefinitely
lazyListOfStrictParsedChunks :: [Chunk] -> [Either String [HashLine]]
lazyListOfStrictParsedChunks cs = tail $ map (fmap fst) $ scanl strictRevChunkParse initial cs
  where
    initial = Right ([], "")

-- TODO any need to also get the header + footer here?
-- TODO put back the maybe depth after basic version works
-- TODO SFO.readFile instead
-- TODO how to properly encapsulate parse errors? maybe MonadThrow/Catch?
-- (for now, fatal error if parsing a chunk fails)
parseTreeFileRev :: OsPath -> IO [HashLine]
parseTreeFileRev f = SFO.withFile f ReadMode $ \h -> do

  -- Find a good block size (how many bytes to a chunk) and calculate where to
  -- start seeking (slightly back from the end at a multiple of the block size
  -- so they line up nicely)
  blksize <- getBlockSize f

  hParseTreeFileRev blksize h

hParseTreeFileRev :: Integer -> Handle -> IO [HashLine]
hParseTreeFileRev blksize h = do
  fileSizeBytes <- hFileSize h
  -- size rounded up to the next block:
  let fileSizeBytesCeiling =
        ceiling (fromInteger fileSizeBytes / fromInteger blksize) * (fromInteger blksize)

  -- read file in block-sized chunks starting from the end
  -- (the first chunk will be shorter than the others; seems not to matter)
  chunks <- makeReverseChunks (fromIntegral blksize) h (fromInteger fileSizeBytesCeiling)
  -- putStrLn $ "n chunks: " ++ show (length chunks)

  -- parse chunks lazily, starting from the end, so they can be streamed into a
  -- tree structure without readnig the entire file first
  let hls = lazyListOfStrictParsedChunks chunks

  -- for now, return parsed HashLines directly and error if any of the parses fail
  fmap concat $ forM hls $ either (error . show) return
