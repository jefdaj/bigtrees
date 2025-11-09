module Cmd.Dupes.Render.Types where

import qualified Data.ByteString.Char8 as B8
import System.Directory.BigTrees (Depth, SortedDupeLists, LogCfg)

-- These could return a single ByteString, but I think it's a little cleaner to
-- use a list because we want to force the evaluation line by line, so we can
-- see the dupes output starting after iteration 1 of simplifyDupes.
-- Note that it's not really a problem for some of the "lines" to be multiline strings;
-- we just care about them being "chunks" of text that can be forced in order.
type DupesRenderFn = LogCfg -> Bool -> Maybe Depth -> SortedDupeLists -> IO [B8.ByteString]
