module Cmd.Info where

import System.IO -- TODO specifics
import Config (Config (..))
import Control.Exception.Safe -- TODO specifics
-- import System.Directory.BigTrees.HashTree
import System.Directory.BigTrees.Hash (Hash(..), prettyHash)
import System.Directory.BigTrees.HashLine (HashLine(..), ErrMsg(..), ModTime(..), NBytes(..), NNodes(..), parseHashLine)
import System.Directory.BigTrees.HashTree (HashTree(..), parseHeader, parseFooter)
import System.Directory.BigTrees.HeadFoot (Header(..), Footer, scanSeconds)
import qualified Data.ByteString.Char8 as B8
import Control.DeepSeq (force)
import Control.Monad (forM)
-- import qualified Data.ByteString.Short as BS

cmdInfo :: Config -> FilePath -> IO ()
cmdInfo cfg path = do
  mH  <- readHeader path
  mLF <- readLastHashLineAndFooter path
  case (mH, mLF) of
    (Just h, Just (l, f)) -> printInfo path h f l
    _ -> error $ "failed to read info from '" ++ path ++ "'"

printInfo :: FilePath -> Header -> Footer -> HashLine -> IO ()
printInfo path header footer lastLine = do
  let seconds = scanSeconds (header, footer)
      lineInfo = case lastLine of
        (ErrLine (_, ErrMsg m,_)) -> ["ERROR: " ++ m]
        (HashLine (_, _, h, ModTime m, NBytes b, NNodes n, _)) ->
          [ "hash " ++ B8.unpack (prettyHash h)
          , "modified " ++ show m
          , show n ++ " files"
          , show b ++ " bytes"
          ]
  mapM_ putStrLn $ path : (map ('\t':) $
    lineInfo ++
    [ "bigtree format " ++ show (treeFormat header)
    -- TODO hash start date
    , "hashing took " ++ show seconds ++ " seconds" -- TODO min, hours, days
    ])

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
