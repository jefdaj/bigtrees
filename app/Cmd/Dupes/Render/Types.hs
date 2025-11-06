module Cmd.Dupes.Render.Types where

import qualified Data.ByteString.Char8 as B8
import System.Directory.BigTrees (Depth, SortedDupeLists, LogCfg)

type DupesRenderFn = LogCfg -> Bool -> Maybe Depth -> SortedDupeLists -> IO B8.ByteString
