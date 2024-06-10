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


--- first attempt at a fold ---

-- attempt1 eventually returns the right result (I think),
-- but doesn't allow doing it lazily.

-- Note that "prev" is the next chunk here. TODO reverse notation?
accHashLines
  :: ([[HashLine]], EndOfPrevChunk)
  -> Chunk
  -> Either String ([[HashLine]], EndOfPrevChunk)
accHashLines (hss, eop) prev = do
  let prev' = B8.append prev $ trace (show $ length hss) eop
  (hs, eop') <- parseOnly parseHashLinesFromChunk prev'
  return (hss ++ [deepseq hs hs], eop')

attempt1 :: [Chunk] -> Either String [[HashLine]]
attempt1 cs = fmap fst $ foldM accHashLines ([], "") cs


--- second attempt ---

-- This time I'm only returning the new hashlines. Will that work?
-- Only the end of the prev chunk is really passed on; the hashlines are
-- accumulated by scanl but would otherwise be thrown away.
acc2
  :: Either String ([HashLine], EndOfPrevChunk)
  -> Chunk
  -> Either String ([HashLine], EndOfPrevChunk) 
acc2 (Left m) _ = Left m
acc2 (Right (_, eop)) prev = 
  let prev' = B8.append prev eop
      res   = parseOnly parseHashLinesFromChunk prev'
  in deepseq res res

attempt2 :: [Chunk] -> [Either String [HashLine]]
attempt2 cs = map (fmap fst) $ scanl acc2 acc cs
  where
    acc = Right ([], "")


--- main ---

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
    -- TODO let hls = map ??? chunks

    -- case parseOnly parseHashLinesFromChunk $ (B8.append (head chunks) "") of
    --   Left msg -> error msg
    --   Right (hs, eop) -> forM_ hs $ B8.putStrLn . prettyLine Nothing

    let hls = attempt2 chunks
    mapM_ (either error $ mapM_ $ B8.putStrLn . prettyLine Nothing) hls

    return ()

  B8.putStrLn "main finish"
