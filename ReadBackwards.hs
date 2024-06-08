{-# LANGUAGE OverloadedStrings #-}

-- Test reading a file backwards by lines, in case that drastically improves tree reading

module Main where

import System.IO
import qualified Data.ByteString.Char8 as B8

f = "/tmp/test-trees/2017-03-27_mono_old-legacy_userhome.tar.lzo.bigtree"

-- create list of data chunks, backwards in order through the file
-- https://stackoverflow.com/a/33853796
makeReverseChunks :: Int -> Handle -> Int -> IO [B8.ByteString]
makeReverseChunks blksize h top
  | top == 0 = return []
  | top < 0  = error "negative file index"
  | otherwise   = do
        let offset = max (top - fromIntegral blksize) 0
        hSeek h AbsoluteSeek (fromIntegral offset)
        blk <- B8.hGet h blksize
        rest <- makeReverseChunks blksize h offset
        return $ blk : rest

main :: IO ()
main = do
  B8.putStrLn "main start"
  withFile f ReadMode $ \h -> do

    fileSizeInBytes <- hFileSize h
    putStrLn $ "size: " ++ show fileSizeInBytes

		-- TODO what's a good idea here? this seems long at first, but there are
    -- 108 chunks in the file so it's probably reasonable
    let blksize = 64*1024 :: Int
    putStrLn $ "blksize: " ++ show blksize

    chunks <- makeReverseChunks blksize h (fromIntegral fileSizeInBytes)

    putStrLn $ show $ B8.length $ head chunks

    return ()
  B8.putStrLn "main finish"
