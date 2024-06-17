{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Minimal test of round-tripping *something* to all possible filenames on disk.

import Data.Attoparsec.ByteString
import Data.Attoparsec.Combinator
import qualified Data.ByteString.Short as SBS
import qualified System.File.OsPath as SFO
import qualified System.OsPath as OSP
import qualified System.OsPath.Internal as OSPI
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString
-- import qualified System.OsString as OSS
import Data.Maybe (fromJust)
import qualified System.Directory.OsPath as SDO
import System.IO.Temp (withSystemTempDirectory)

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
  bs1 <- generate ((arbitrary :: Gen SBS.ShortByteString) `suchThat` isValidName)
  bs2 <- generate ((arbitrary :: Gen SBS.ShortByteString) `suchThat` isValidName)
  putStrLn $ show bs1
  putStrLn $ show bs2

  -- converting to ospath isn't hard, as long as you're ok doing it outside parser monad?
  op1 <- OSPI.fromBytes $ SBS.fromShort bs1
  op2 <- OSPI.fromBytes $ SBS.fromShort bs2
  putStrLn $ show op1
  putStrLn $ show op2

  -- TODO but instead can you just "cast" it directly with a hidden constructor?
  -- op2 = ...

  -- round-trip to filename
  withSystemTempDirectory "roundtripnames" $ \tmpDir -> do
    tmpDir' <- OSP.encodeUtf tmpDir
    let tmpInner = tmpDir' OSP.</> op1
    let tmpPath = tmpInner OSP.</> op2
    putStrLn $ show tmpPath
    let txt = "this is a test"

    -- test: should be able to create the dir
    SDO.createDirectoryIfMissing False tmpInner

    -- test: should be able to see it in the outer tmpdir
    cs1 <- SDO.getDirectoryContents tmpDir'
    putStrLn $ "tmpdir present? " ++ show (op1 `elem` cs1)
    putStrLn $ show cs1

    -- test: should be able to write the file to the inner tmpdir
    SFO.writeFile tmpPath txt

    -- test: should be able to read dir contents of inner tmpdir
    -- test: should be able to find the file in the inner dir
    cs2 <- SDO.getDirectoryContents tmpInner
    putStrLn $ "filename present? " ++ show (op2 `elem` cs2)
    putStrLn $ show cs2

    -- test: should be able to read the file contents back
    txt' <- SFO.readFile tmpPath
    putStrLn $ "txt == txt'? " ++ show (txt == txt')

    return ()

  -- TODO ok, do need to convert -> OsPath in order to write a file I guess?
  -- maybe it's a good time to do the hidden import black magic thing?
  return ()
