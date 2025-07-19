{-# LANGUAGE OverloadedStrings   #-}

module Cmd.Dupes.Render.TestScript where

import Cmd.Dupes.Render.Types
import qualified Data.ByteString.Char8 as B8
import System.Directory.BigTrees
import System.OsPath (OsPath, (</>), joinPath, splitDirectories, decodeFS)

-- type DupesRenderFn = Bool -> Maybe Depth -> SortedDupeLists -> IO B8.ByteString

renderTestScript :: DupesRenderFn
renderTestScript keepOne md ls = do
  body <- mapM excludeLines ls
  return $ B8.unlines $ fileHeader : body
  where

    fileHeader = B8.pack $
      "# This is the 'test-script' output format.\n\
      \# It's mainly for debugging.\n"

    depthWarning Nothing  = ""
    depthWarning (Just (Depth d)) =
      " (up to " `B8.append` B8.pack (show d) `B8.append` " levels deep)"

    excludeLines :: DupeList -> IO B8.ByteString
    excludeLines (n, h, t, paths) = do
      paths' <- mapM decodeFS paths
      return $ B8.unlines
             $ groupHeader h t n (length paths)
             : (map B8.pack $ sortPaths paths')

    groupHeader :: Hash -> TreeType -> Int -> Int -> B8.ByteString
    groupHeader _ E _ _ = "" -- TODO is that a good idea?
    groupHeader h D nSaved nDirs = B8.intercalate " "
      [ "#", B8.pack (show nDirs)
      , "duplicate directories with hash"
      , prettyHash h `B8.append` (depthWarning md)
      ]
    groupHeader h F nSaved nFiles = B8.intercalate " "
      [ "#", B8.pack (show nFiles)
      , "duplicate files with hash"
      , prettyHash h `B8.append` (depthWarning md)
      ]
    groupHeader h _ nSaved nLinks = B8.intercalate " "
      [ "#", B8.pack (show nLinks)
      , "duplicate links with hash"
      , prettyHash h `B8.append` (depthWarning md)
      ]
