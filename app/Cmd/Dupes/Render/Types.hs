module Cmd.Dupes.Render.Types where

import System.Directory.BigTrees (Depth, SortedDupeLists)
import qualified Data.ByteString.Char8 as B8

type DupesRenderFn = Bool -> Maybe Depth -> SortedDupeLists -> IO B8.ByteString
