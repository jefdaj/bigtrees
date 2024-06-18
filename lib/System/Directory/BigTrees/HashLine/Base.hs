module System.Directory.BigTrees.HashLine.Base
  ( hashLineFields
  , sepChar
  , joinCols
  , nullP
  , nullBreakP
  )
  where

import qualified Data.ByteString.Char8 as B8
import Control.Monad (void)
import Data.Attoparsec.ByteString.Char8 (Parser, char, endOfLine)

sepChar :: Char
sepChar = '\t'

joinCols :: [B8.ByteString] -> B8.ByteString
joinCols = B8.intercalate (B8.singleton sepChar)

-- TODO use this more directly?
-- For now it's only imported by HeadFoot to use in the Header
hashLineFields :: [String]
hashLineFields = ["type", "depth", "hash", "modtime", "nbytes", "nfiles", "name"]

-- TODO move to Name? Util?
nullP :: Parser ()
nullP = void $ char '\NUL'

-- TODO rename? confusingly sounds "bigger" than breakP
nullBreakP :: Parser ()
nullBreakP = nullP *> endOfLine
