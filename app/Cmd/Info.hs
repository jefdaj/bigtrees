module Cmd.Info where

import System.IO -- TODO specifics
import Config (Config (..))
-- import System.Directory.BigTrees.HashTree

cmdInfo :: Config -> FilePath -> IO ()
cmdInfo cfg path = do
  putStrLn "cmdInfo"
  putStrLn $ "path: " ++ show path
  putStrLn $ "cfg: " ++ show cfg
  -- TODO parse and print header without reading much of the file
  -- TODO parse and print footer from the end, again without reading the middle
  -- TODO calculate scan time
  -- TODO also pick up some stats from the last line of the body

-- https://stackoverflow.com/a/41658016
hGetLastLines :: Handle -> IO String
hGetLastLines hdl = go "" (-1)
  where
  go s i = do
    hSeek hdl SeekFromEnd i
    c <- hGetChar hdl
    if c == '\n'
      then pure s
      else go (c:s) (i-1)

-- Option 1:
-- skip final empty line if any
-- accumulate as long as lines start with #
-- also get the first line that doesn't

-- Option 2:
-- search back by char until the pattern:
-- new line starts with something other than '#'

-- TODO handle the case where we get to the beginning of the file?
-- TODO why is 2 required rather than 1?
--
-- Example usage:
--
-- >>> withFile "stack-work.bigtree"  ReadMode $ hTakePrevUntil (\s -> "\n# {" `isPrefixOf` s)
-- "\n# {\n#   \"scanEnd\": 1717518711\n#"
--
hTakePrevUntil :: (String -> Bool) -> Handle -> IO String
hTakePrevUntil pred hdl = do
  hSeek hdl SeekFromEnd (-2)
  hTakePrevUntil' pred hdl ""

-- The internal helper that also takes a seek position and the accumulated
-- string from that point on. Note that the integer index should be negative.
-- TODO negate so the index is positive?
-- TODO why is 2 required rather than 1?
hTakePrevUntil' :: (String -> Bool) -> Handle -> String -> IO String
hTakePrevUntil' pred hdl cs = do
  hSeek hdl RelativeSeek (-2)
  c <- hGetChar hdl
  -- n <- hGetPosn hdl
  -- putStrLn $ show n ++ " " ++ [c]
  let cs' = c:cs
  if pred cs'
    then pure cs'
    else hTakePrevUntil' pred hdl cs'
