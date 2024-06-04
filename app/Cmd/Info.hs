module Cmd.Info where

import System.IO -- TODO specifics
import Config (Config (..))
import Control.Exception.Safe -- TODO specifics
-- import System.Directory.BigTrees.HashTree
import System.Directory.BigTrees.HashLine (HashLine, parseHashLine)
import System.Directory.BigTrees.HashTree (parseHeader, parseFooter)
import System.Directory.BigTrees.HeadFoot (Header, Footer)
import qualified Data.ByteString.Char8 as B8
import Control.DeepSeq (force)
import Control.Monad (forM)

cmdInfo :: Config -> FilePath -> IO ()
cmdInfo cfg path = do
  putStrLn "cmdInfo"
  putStrLn $ "path: " ++ show path
  putStrLn $ "cfg: " ++ show cfg
  -- TODO parse and print header without reading much of the file
  -- TODO parse and print footer from the end, again without reading the middle
  -- TODO calculate scan time
  -- TODO also pick up some stats from the last line of the body

--- read header info from the beginning of the file ---

-- TODO document 100 line limit
readHeader :: FilePath -> IO (Maybe Header)
readHeader path =
  withFile path ReadMode $ \h -> do
    commentLines <- fmap (takeWhile isCommentLine) $ forM [1..100] $ \_ -> hGetLine h
    return $ parseHeader commentLines

isCommentLine :: String -> Bool
isCommentLine ('#':_) = True
isCommentLine _ = False

--- read summary info from the end of the file ---

-- TODO move to HeadFoot? HashTree.Read?
-- TODO factor out/document the max char thing
readLastHashLineAndFooter :: FilePath -> IO (Maybe (HashLine, Footer))
readLastHashLineAndFooter path = do
  mTxt <- withFile path ReadMode $ hTakePrevUntil isDepthZeroLine 10000
  case mTxt of
    Nothing -> return Nothing
    Just txt -> case filter (not . null) $ lines txt of
      [] -> return Nothing
      [_] -> return Nothing
      (l:ls) -> do
        let ml = parseHashLine $ B8.pack $ l
            mf = parseFooter $ ls
        case (ml, mf) of
          (Just l, Just f) -> return $ Just (l, f)
          _ -> return Nothing

-- Tests whether the string looks like a newline + HashLine with Depth 0
isDepthZeroLine :: String -> Bool
isDepthZeroLine ('\n':_:'\t':'0':'\t':_) = True
isDepthZeroLine _ = False

-- Note that max is a positive number, the max chars to take,
-- whereas the seek index is a negative number from the end.
-- TODO handle the case where we get to the beginning of the file?
--
-- Example usage:
--
-- >>> withFile "stack-work.bigtree"  ReadMode $ hTakePrevUntil (\s -> "\n# {" `isPrefixOf` s) 100
-- "\n# {\n#   \"scanEnd\": 1717518711\n#"
--
-- >>> withFile "stack-work.bigtree"  ReadMode $ hTakePrevUntil (\s -> "\n# {" `isPrefixOf` s) 10
-- Nothing
--
hTakePrevUntil :: (String -> Bool) -> Int -> Handle -> IO (Maybe String)
hTakePrevUntil pred max hdl = handleAnyDeep (\_ -> return Nothing) $ do
  hSeek hdl SeekFromEnd 0
  hTakePrevUntil' pred max hdl ""

-- Internal helper that also takes a seek position and the accumulated string.
hTakePrevUntil' :: (String -> Bool) -> Int -> Handle -> String -> IO (Maybe String)
hTakePrevUntil' _ max _ _ | max < 0 = return Nothing
hTakePrevUntil' pred max hdl cs = do
  hSeek hdl RelativeSeek (-2)
  c <- hGetChar hdl
  let cs' = c:cs
  if pred cs'
    then return $ Just cs'
    else hTakePrevUntil' pred (max-1) hdl cs'
