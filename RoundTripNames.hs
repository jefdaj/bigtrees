{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Minimal test of round-tripping *something* to all possible filenames on disk.

import Data.Attoparsec.ByteString
import Data.Attoparsec.Combinator
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString
import qualified Data.ByteString.Short as SBS
import qualified System.File.OsPath as SFO
import qualified System.OsPath as OSP
-- import qualified System.OsString as OSS

-- instance Arbitrary OSP.OsPath where
  -- arbitrary = <$> ((arbitrary :: Gen SBS.ShortByteString) `suchThat` isValidName)

-- instance Arbitrary Name where
--   arbitrary = Name <$> (arbitrary `suchThat` isValidName)

isValidName :: SBS.ShortByteString -> Bool
isValidName b
  =  notElem b ["", ".", ".."]
  && (not $ "/" `SBS.isInfixOf` b) -- the byte should be \x2f or 47
  && (not $ "\NUL" `SBS.isInfixOf` b)

main :: IO ()
main = do
  name <- generate ((arbitrary :: Gen SBS.ShortByteString) `suchThat` isValidName)
  putStrLn $ show name
  return ()
