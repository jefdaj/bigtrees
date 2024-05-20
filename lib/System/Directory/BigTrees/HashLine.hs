{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module System.Directory.BigTrees.HashLine

  ( HashLine(..)
  , TreeType(..)
  , IndentLevel(..)
  -- , Hash(..) TODO re-export here? And Name too?
  , prettyHashLine
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
import System.Directory.BigTrees.Name (Name (..), fp2n)
import TH.Derive (Deriving, derive)

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

-- TODO actual Pretty instance
-- TODO avoid encoding as UTF-8 if possible; use actual bytestring directly
-- note: p can have weird characters, so it should be handled only as ByteString
prettyHashLine :: HashLine -> B8.ByteString
prettyHashLine (HashLine (t, IndentLevel n, h, Name p)) = B8.unwords
  [B8.pack $ show t, B8.pack $ show n, prettyHash h, T.encodeUtf8 p] -- TODO mismatch with n2fp, fp2n?

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
