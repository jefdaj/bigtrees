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

import Debug.Trace

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

-- https://stackoverflow.com/a/25533374
-- skipToNextBreak :: Parser ()
-- skipToNextBreak = skipWhile undefined

type EndOfPrevChunk = B8.ByteString
type Chunk          = B8.ByteString

parseHashLinesFromChunk :: Parser ([HashLine], EndOfPrevChunk)
parseHashLinesFromChunk = do
  eop <- endofprevP
  hls <- reverse <$> linesP Nothing
  return (hls, eop) -- TODO right spot to deepseq?

-- Note that "prev" is the next chunk here. TODO reverse notation?
accHashLines
  :: ([[HashLine]], EndOfPrevChunk)
  -> Chunk
  -> Either String ([[HashLine]], EndOfPrevChunk)
accHashLines (hss, eop) prev = do
  let prev' = B8.append prev $ trace (show $ map length hss) eop
  (hs, eop') <- parseOnly parseHashLinesFromChunk prev'
  return (hss ++ [deepseq hs hs], eop')

-- accHashLines' hs eop next = undefined

foldOverChunks :: [Chunk] -> Either String [[HashLine]]
foldOverChunks cs = fmap fst $ foldM accHashLines ([], "") cs

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

    -- case parseOnly parseHashLinesFromChunk $ (B8.append (head chunks) "") of
    --   Left msg -> error msg
    --   Right (hs, eop) -> forM_ hs $ B8.putStrLn . prettyLine Nothing

    -- case foldM accHashLines ([], "") chunks of
    --  Left msg -> error msg
    --   Right (hs, eop) -> forM_ hs $ B8.putStrLn . prettyLine Nothing

    -- let hls = foldr accHashLines ([], "") chunks
    -- mapM_ (either error (B8.putStrLn . prettyLine Nothing)) hls

    -- case foldOverChunks chunks of
    --   Left msg -> error msg
    --   Right hss -> forM_ hss $ \hs -> mapM_ (B8.putStrLn . prettyLine Nothing) hs

    forM_ (foldOverChunks chunks) $ \hs -> 
      mapM_ (putStrLn . show) hs

    -- TODO tentative algorithm:
    --      1. find the first break(P) in a chunks
    --      2. cut off everything before that and keep it to append to the next chunk
    --      3. if there's a prev cut off part, append to the current chunk before parsing
    --      3. parse with hashLineP/linesP
    --      4. deepseq each thunk's list of hashlines, then reverse them
    --      5. although chunks are strict, at the top level we want the list of them to be lazy

    return ()

  B8.putStrLn "main finish"
