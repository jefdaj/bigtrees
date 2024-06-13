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
import qualified System.OsPath.Internal as OSPI
-- import qualified System.OsString as OSS
import System.IO.Temp (withSystemTempDirectory)
import qualified System.Directory.OsPath as SDO

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

  -- generating them isn't hard
  bs <- generate ((arbitrary :: Gen SBS.ShortByteString) `suchThat` isValidName)
  putStrLn $ show bs

  -- converting to ospath isn't hard, as long as you're ok doing it outside parser monad?
  op1 <- OSPI.fromBytes $ SBS.fromShort bs
  putStrLn $ show op1

  -- TODO but instead can you just "cast" it directly with a hidden constructor?
  -- op2 = ...

  -- round-trip to filename
  withSystemTempDirectory "roundtripnames" $ \tmpDir -> do
    tmpDir' <- OSP.encodeUtf tmpDir 
    let tmpPath = tmpDir' OSP.</> op1
    putStrLn $ show tmpPath
    let txt = "this is a test"

    -- test: should be able to write the file
    SFO.writeFile tmpPath txt

    -- test: should be able to find the file in the dir
    cs <- SDO.getDirectoryContents tmpDir'
    putStrLn $ "filename present? " ++ show (op1 `elem` cs)
    putStrLn $ show cs

    -- test: should be able to read the file contents back
    txt' <- SFO.readFile tmpPath
    putStrLn $ "txt == txt'? " ++ show (txt == txt')

    return ()

  -- TODO ok, do need to convert -> OsPath in order to write a file I guess?
  -- maybe it's a good time to do the hidden import black magic thing?
  return ()
