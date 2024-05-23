{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module System.Directory.BigTrees.HashLine

  ( HashLine(..)
  , TreeType(..)
  , IndentLevel(..)
  -- , Hash(..) TODO re-export here? And Name too?
  , prettyLine
  , parseHashLine -- TODO remove? not actually used
  , parseHashLines

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
import qualified Data.Text.Encoding as T
import Prelude hiding (take)
import System.Directory.BigTrees.Hash (Hash (Hash), digestLength, prettyHash)
import System.Directory.BigTrees.Name (Name (..), fp2n, n2fp, breadcrumbs2fp)
import Test.QuickCheck (Arbitrary (..), Gen, choose, resize, sized, suchThat)
import TH.Derive ()
import System.FilePath ((</>))

-----------
-- types --
-----------

-- for distinguishing beween files and dirs
data TreeType = D | F
  deriving (Eq, Ord, Read, Show)

instance NFData TreeType
  where rnf :: TreeType -> ()
        rnf = const () -- TODO is this valid?

newtype IndentLevel
  = IndentLevel Int
  deriving (Eq, Ord, Read, Show)

-- TODO make a skip type here, or in hashtree?
-- TODO remove the tuple part now?
newtype HashLine
  = HashLine (TreeType, IndentLevel, Hash, Name)
  deriving (Eq, Ord, Read, Show)

---------------
-- instances --
---------------

-- TODO remove?
instance Arbitrary IndentLevel where

  arbitrary :: Gen IndentLevel
  arbitrary = IndentLevel <$> ((arbitrary :: Gen Int) `suchThat` (>= 0))

  shrink :: IndentLevel -> [IndentLevel]
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
    il <- arbitrary :: Gen IndentLevel
    h  <- arbitrary :: Gen Hash
    n  <- arbitrary :: Gen Name
    return $ HashLine (tt, il, h, n)

  -- only shrinks the filename
  -- TODO also change the treetype?
  shrink :: HashLine -> [HashLine]
  shrink (HashLine (tt, il, h, n)) = map (\n' -> HashLine (tt, il, h, n')) (shrink n)

-----------
-- print --
-----------

-- TODO actual Pretty instance
-- TODO avoid encoding as UTF-8 if possible; use actual bytestring directly
-- TODO rename/move this? it's used in printing lines and also find paths
-- TODO make this a helper and export 2 fns: prettyHashLine, prettyPathLine?
-- note: p can have weird characters, so it should be handled only as ByteString
prettyLine :: Maybe [Name] -> HashLine -> B8.ByteString
prettyLine breadcrumbs (HashLine (t, IndentLevel n, h, name)) =
  let node = case breadcrumbs of
               Nothing -> n2fp name
               Just ns -> breadcrumbs2fp $ name:ns
  in B8.unwords
       -- TODO make the metadata configurable here?
       [ B8.pack $ show t
       , B8.pack $ show n
       , prettyHash h
       , B8.pack node -- TODO n2b?
       ]

------------
-- parser --
------------

-- TODO rewrite a lot of this to deal flexibly with tables? or will order stay fixed?

typeP :: Parser TreeType
typeP = do
  t <- choice [char 'D', char 'F'] <* char ' '
  return $ read [t]

hashP :: Parser Hash
hashP = do
  h <- take digestLength -- TODO any need to sanitize these?
  _ <- char ' '
  return $ Hash $ BS.toShort h

{- Like endOfLine, but make sure D/F comes next followed by a valid hash digest
 - instead of the rest of a filename. This catches the rare case where a
 - filename contains a newline followed by D or F. You could still construct a
 - filename that would fool it, but it would be extremely unlikely to happen by
 - chance.
 - TODO can it use null-separated lines instead like -print0?
 -}
breakP :: Parser ()
breakP = endOfLine >> choice [typeP >> indentP >> hashP >> return (), endOfInput]

-- TODO should anyChar be anything except forward slash and the null char?
nameP :: Parser Name
nameP = fmap fp2n $ do
  c  <- anyChar
  cs <- manyTill anyChar $ lookAhead breakP
  return (c:cs)

indentP :: Parser IndentLevel
indentP = do
  n <- manyTill digit $ char ' '
  -- TODO char ' ' here?
  return $ IndentLevel $ read n

-- TODO is there a cleaner syntax for this?
-- TODO this should still count up total files when given a max depth
lineP :: Maybe Int -> Parser (Maybe HashLine)
lineP md = do
  t <- typeP
  (IndentLevel i) <- indentP
  case md of
    Nothing -> parseTheRest t (IndentLevel i)
    Just d -> do
      if i > d
        then do
          skipWhile (not . isEndOfLine)
          lookAhead breakP
          return Nothing
        else parseTheRest t (IndentLevel i)
  where
    parseTheRest t i = do
      h <- hashP
      p <- nameP
      -- return $ trace ("finished: " ++ show (t, i, h, p)) $ Just (t, i, h, p)
      return $ Just (HashLine (t, i, h, p))

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
