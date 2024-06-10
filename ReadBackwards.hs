{-# LANGUAGE OverloadedStrings #-}

-- Test reading a file backwards by lines, in case that drastically improves tree reading

module Main where

import System.IO
import qualified Data.ByteString.Char8 as B8
import System.Directory.BigTrees
import System.Directory.BigTrees.HashLine
import System.Directory.BigTrees.HashTree.Read
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator
import Control.Monad (forM_, foldM)
import Control.DeepSeq (deepseq)

-- Return all the text before the next hashline break, which should be a
-- partial line, so it can be appended to the next chunk and properly parsed
-- there.
endofprevP :: Parser B8.ByteString
endofprevP = fmap B8.pack $ (manyTill anyChar $ lookAhead breakP) <* endOfLine

f = "2022-02-17_arachno-dom0-annex.tar.lzo.bigtree"

-- create list of data chunks, backwards in order through the file
-- https://stackoverflow.com/a/33853796
makeReverseChunks :: Int -> Handle -> Int -> IO [Chunk]
makeReverseChunks blksize h top
  | top == 0 = return []
  | top < 0  = error "negative file index"
  | otherwise   = do
        let offset = max (top - fromIntegral blksize) 0
        hSeek h AbsoluteSeek (fromIntegral offset)
        blk <- B8.hGet h blksize
        rest <- makeReverseChunks blksize h offset
        return $ blk : rest

type EndOfPrevChunk = B8.ByteString
type Chunk          = B8.ByteString

parseHashLinesFromChunk :: Parser ([HashLine], EndOfPrevChunk)
parseHashLinesFromChunk = do
  eop <- endofprevP
  hls <- reverse <$> linesP Nothing
  return (hls, eop)

-- The list of lines here is only used by scanl, not inside this fn;
-- the end of prev chunk is only used inside this fn and ignored by scanl.
-- TODO come up with a better way of handling Left besides infinite recursion
strictParseChunkHelper
  :: Either String ([HashLine], EndOfPrevChunk)
  -> Chunk
  -> Either String ([HashLine], EndOfPrevChunk)
strictParseChunkHelper (Left m) _ = Left m
strictParseChunkHelper (Right (_, eop)) prev =
  let prev' = B8.append prev eop
      res   = parseOnly parseHashLinesFromChunk prev'
  in deepseq res res

-- This returns a lazy list of chunk parse results, but each one will fully evaluate
-- once accessed.
-- WARNING once it hits an error (Left), it will keep repeating that error indefinitely
lazyListOfStrictParsedChunks :: [Chunk] -> [Either String [HashLine]]
lazyListOfStrictParsedChunks cs = map (fmap fst) $ scanl strictParseChunkHelper initial cs
  where
    initial = Right ([], "")

main :: IO ()
main = do
  B8.putStrLn "main start"

  withFile f ReadMode $ \h -> do

    fileSizeInBytes <- hFileSize h
    putStrLn $ "size: " ++ show fileSizeInBytes

    -- TODO what's a good size here? can I pick it up from a system call?
    let blksize = 64*1024 :: Int
    putStrLn $ "blksize: " ++ show blksize

    chunks <- makeReverseChunks blksize h (fromIntegral fileSizeInBytes)

    -- This is an odd pattern, but seems to work alright.
    -- The main weirdness is that if we were to ignore a Left rather than erroring,
    -- it would then repeat that Left infinitely.
    -- TODO think about whether there's a better idiom for this
    let hls = lazyListOfStrictParsedChunks chunks
    mapM_ (either error $ mapM_ $ B8.putStrLn . prettyLine Nothing) hls

    return ()

  B8.putStrLn "main finish"
