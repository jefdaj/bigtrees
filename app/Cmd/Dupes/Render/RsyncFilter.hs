{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Cmd.Dupes.Render.RsyncFilter where

import Cmd.Dupes.Render.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import qualified Data.List.Split as LS
import Data.Word (Word8)
import System.Directory.BigTrees
import System.OsPath (OsPath, decodeFS, joinPath, osp, splitDirectories, (</>))

replaceTopDirWithSlash :: OsPath -> OsPath
replaceTopDirWithSlash path = joinPath comps'
  where
    comps = splitDirectories path
    comps' = if length comps < 2 then comps else [osp|/|] : tail comps -- TODO is this right?

-- Function to escape specific special characters directly in ByteString
-- TODO is this all? or does it need the wildcard mode thing as before?
escapeRsyncPathBytes :: B8.ByteString -> B8.ByteString
escapeRsyncPathBytes bs = B8.concatMap escapeRsyncPathByte bs

escapeRsyncPathByte :: Char -> B8.ByteString
escapeRsyncPathByte b
  | b == '*'  = B8.pack "\\*"
  | b == '?'  = B8.pack "\\?"
  | b == '['  = B8.pack "\\["
  | otherwise = B8.singleton b

renderRsyncFilter :: DupesRenderFn
renderRsyncFilter keepOne md ls = do
  body <- mapM groupDupes ls
  return $ B8.unlines $ fileHeader : body ++ catchall
  where

    fileHeader = B8.pack $
      "# This is the 'rsync-filter-file' output format.\n\
      \# You can use it to tell rsync all the files *not* to copy.\n\
      \# Example rsync command:\n\
      \#\n\
      \# rsync -arv SRCDIR/ DSTDIR/ --filter 'merge THISFILE'\n\
      \#\n\
      \# Where SRCDIR is your originally scanned folder with duplicates,\n\
      \# DSTDIR is the new, non-duplicated copy you'll be making,\n\
      \# and THISFILE is where you saved the output of this command.\n"
      ++ (if keepOne then "" else
      "#\n\
      \# !!! WARNING !!!\n\
      \# Since you're deduping vs a reference set, ALL dupes will be listed\n\
      \# in the exclude file. The assumption is that you already have another copy\n\
      \# saved somewhere else, and that was used to generate the reference set.\n")
      ++
      "#\n\
      \# Note that the trailing slashes in the rsync command above and the\n\
      \# leading slashes in each filename below are important.\n\
      \#\n\
      \# The very last line is also important. It's a catchall rule that says\n\
      \# 'copy everything not matching one of the exclude patterns'.\n\
      \#\n\
      \# You might want to try running the rsync command with --dry-run at the end\n\
      \# first to make sure it does what you expected.\n"

    depthWarning Nothing  = ""
    depthWarning (Just (Depth d)) =
      " (up to " `B8.append` B8.pack (show d) `B8.append` " levels deep)"

    groupDupes :: DupeList -> IO B8.ByteString
    groupDupes (n, h, t, paths) = do
      let paths'    = map (\(a, b) -> (a, replaceTopDirWithSlash b)) paths
          paths''   = map (escapeRsyncPathBytes . op2bs . snd) $ sortPaths paths'
          paths'''  = if t == D then map (<> "/") paths'' else paths''
          paths'''' = if not keepOne
                       then map ("- " <>) paths'''
                       else ("+ " <> head paths'''):map ("- " <>) (tail paths''')
      return $ B8.unlines $ groupHeader h t n (length paths) : paths''''

    nSkip ds = B8.pack $ show $ if keepOne then ds - 1 else ds

    exclude ds
      | not keepOne = "exclude"
      | ds > 2 = "exclude all but one of"
      | otherwise = "exclude one of"

    plural :: Int -> B8.ByteString -> B8.ByteString
    plural n thing = if n > 1 then thing `B8.append` "s" else thing

    -- TODO don't mention inodes unless it's a dir, so separate fn for that
    explain :: Hash -> Int -> Int -> B8.ByteString -> B8.ByteString
    explain h nSaved nThings thing = B8.intercalate " "
      [ "#", exclude nThings
      , "these", B8.pack $ show nThings
      , "duplicate", thing `B8.append` "s"
      , "with hash", prettyHash h <> ","
      , "saving", B8.pack $ show nSaved
      , (plural nSaved "inode")
      ]

    groupHeader :: Hash -> TreeType -> Int -> Int -> B8.ByteString
    groupHeader _ E _ _           = "" -- TODO is that a good idea?
    groupHeader h D nSaved nDirs  = explain h nSaved nDirs "folder"
    groupHeader h F nSaved nFiles = explain h nSaved nFiles "file"
    groupHeader h _ nSaved nLinks = explain h nSaved nLinks "link"

    catchall :: [B8.ByteString]
    catchall =
      [ "# Finally, copy everything not matching one of the filters"
      , "+ *"
      ]
