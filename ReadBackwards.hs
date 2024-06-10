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

-- like regular breakP, except "no comments": it doesn't recognize '#'
breakPNC :: Parser ()
breakPNC = endOfLine >> choice [typeP >> numStrP >> hashP >> numStrP >> return (), endOfInput]

-- Return all the text before the next hashline break, which should be a
-- partial line, so it can be appended to the next chunk and properly parsed
-- there.
endofprevP :: Parser B8.ByteString
endofprevP = fmap B8.pack $ (manyTill anyChar $ lookAhead breakPNC) <* endOfLine

f = "yesod-blog.tar.bigtree"
-- f = "2022-02-17_arachno-dom0-annex.tar.lzo.bigtree"

-- create list of data chunks, backwards in order through the file
-- https://stackoverflow.com/a/33853796
makeReverseChunks :: Int -> Handle -> Int -> IO [Chunk]
makeReverseChunks blksize h end
  | end == 0 = return []
  | end < 0  = error "negative file index"
  | otherwise   = do
        let start = max (end - fromIntegral blksize) 0
        hSeek h AbsoluteSeek (fromIntegral start)
        blk <- B8.hGet h (end - start)
        rest <- makeReverseChunks blksize h start
        -- return $ (trace ("blk " ++ show start ++ "-" ++ show end ++ ":" ++ show blk) blk) : rest
        return $ blk : rest

type EndOfPrevChunk = B8.ByteString
type Chunk          = B8.ByteString

-- TODO pass maybe max depth here
parseHashLinesFromChunk :: Parser ([HashLine], EndOfPrevChunk)
parseHashLinesFromChunk = do

  -- if this is the first chunk in the file (last in iteration),
  -- there will be a header to skip before the lines start.
  -- 
  -- this was working better before when it was just sepBy' commentLineP endOfLine,
  -- but i worry that might swallow any line that happens to start with '#'
  --
  _ <- option undefined headerP -- TODO undefined should be safe here, no?

  -- if this is the second-to-last chunk and it happens to start in the middle of the header,
  -- the easiest thing to do is pass that to the very last chunk as part of eop
  eop <- endofprevP

  hls <- reverse <$> linesP Nothing
  -- same with the footer, if this is the final chunk in the file (first read)
  -- _ <- option [] $ sepBy' commentLineP endOfLine
  -- _ <- option [] $ many' endOfLine

  _ <- option undefined footerP

  remain <- manyTill anyChar endOfInput
  _ <- endOfInput
  -- let tfn x = if null remain then x else trace ("remain: '" ++ remain ++ "'") x
  -- return $ tfn $ (hls, eop)
  return (hls, eop)

-- The list of lines here is only used by scanl, not inside this fn;
-- the end of prev chunk is only used inside this fn and ignored by scanl.
-- TODO come up with a better way of handling Left besides infinite recursion
strictRevChunkParse
  :: Either String ([HashLine], EndOfPrevChunk)
  -> Chunk
  -> Either String ([HashLine], EndOfPrevChunk)
strictRevChunkParse (Left m) _ = Left m
strictRevChunkParse (Right (_, eop)) prev =
  let prev' = B8.append prev $ B8.append eop "\n" -- TODO what about newline before eop here??
      res   = case parseOnly parseHashLinesFromChunk prev' of
                Left "not enough input" -> Right ([], "")
                x -> x
  in deepseq res res

-- This returns a lazy list of chunk parse results, but each one will fully evaluate
-- once accessed.
-- WARNING once it hits an error (Left), it will keep repeating that error indefinitely
lazyListOfStrictParsedChunks :: [Chunk] -> [Either String [HashLine]]
lazyListOfStrictParsedChunks cs = tail $ map (fmap fst) $ scanl strictRevChunkParse initial cs
  where
    initial = Right ([], "")

main :: IO ()
main = do
  B8.putStrLn "main start"

  withFile f ReadMode $ \h -> do
    withFile (f ++ ".after") WriteMode $ \h2 -> do

      fileSizeInBytes <- hFileSize h
      putStrLn $ "size: " ++ show fileSizeInBytes

      -- TODO what's a good size here? can I pick it up from a system call?
      let blksize = 1*1024 :: Int -- TODO changing this uncovers bugs?
      putStrLn $ "blksize: " ++ show blksize

      chunks <- makeReverseChunks blksize h (fromIntegral fileSizeInBytes)
      -- putStrLn $ "n chunks: " ++ show (length chunks)
      -- putStrLn $ show $ Prelude.head chunks

      -- This is an odd pattern, but seems to work alright.
      -- The main weirdness is that if we were to ignore a Left rather than erroring,
      -- it would then repeat that Left infinitely.
      -- TODO think about whether there's a better idiom for this
      let hls = lazyListOfStrictParsedChunks chunks
      putStrLn $ show hls
      mapM_ (either error $ mapM_ $ B8.hPutStrLn h2 . prettyLine Nothing) hls

      return ()

  B8.putStrLn "main finish"
