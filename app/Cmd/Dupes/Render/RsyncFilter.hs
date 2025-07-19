module Cmd.Dupes.Render.RsyncFilter where

import Cmd.Dupes.Render.Types
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.ByteString.Char8 as B8
import System.Directory.BigTrees
import System.OsPath (OsPath, (</>), joinPath, splitDirectories, decodeFS)

-- escapeRsyncExcludeSpecialChars :: String -> String
-- escapeRsyncExcludeSpecialChars input = concatMap escapeChar input
--   where
--     specialChars = "*?#\!()" :: String
--     escapeChar c
--       | c `L.elem` specialChars = '\\' : [c]
--       | otherwise  = [c]

replaceTopDirWithSlash :: String -> String
replaceTopDirWithSlash path = '/' : L.intercalate "/" pathTail
  where
    comps = LS.splitOn "/" path
    pathTail = if null comps then [] else tail comps

escapeRsyncExcludeFromPath2 :: String -> String
escapeRsyncExcludeFromPath2 path = if wildcardMode then escaped else path
  where

    -- and if the path starts with # that needs to be escaped to prevent being
    -- treated as a comment
    -- TODO but that never happens here because we prepend /, right?

    -- if path has one of these, rsync will treat it as a pattern;
    -- if not, everything is matched literally and \ etc will break it!
    wildcardMode = any (`L.elem` path) wildcardTriggerChars
    wildcardTriggerChars = "*?[" :: String

    -- Once wildcard mode is triggered, these chars need escaping:
    -- TODO verify each one!
    escaped = concatMap escapeChar path
    -- specialChars = "*?#\\!()" :: String
    specialChars = "*?[\\" :: String
    escapeChar c
      | c `L.elem` specialChars = '\\' : [c]
      | otherwise  = [c]

renderDupesRsyncFilter :: DupesRenderFn
renderDupesRsyncFilter keepOne md ls = do
  body <- mapM groupDupes ls
  return $ B8.unlines $ fileHeader : body
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
      \# WARNING!\n\
      \# Since you're deduping vs a reference set, ALL dupes will be listed\n\
      \# in the exclude file. The assumption is that you already have another copy\n\
      \# saved somewhere else, and that was used to generate the reference set.\n")
      ++
      "#\n\
      \# Note that the trailing slashes in the rsync command above and the\n\
      \# leading slashes in each filename below are important.\n\
      \#\n\
      \# You might want to try the command with --dry-run at the end first\n\
      \# to make sure it does what you expected!\n"

    depthWarning Nothing  = ""
    depthWarning (Just (Depth d)) =
      " (up to " `B8.append` B8.pack (show d) `B8.append` " levels deep)"

    groupDupes :: DupeList -> IO B8.ByteString
    groupDupes (n, _, t, paths) = do
      paths' <- mapM decodeFS paths -- TODO is decoding necessary, even to write a script?
      let paths''   = sortPaths $ map (escapeRsyncExcludeFromPath2 . replaceTopDirWithSlash) paths'
          paths'''  = if t == D then map (++ "/") paths'' else paths''
          paths'''' = if not keepOne
                       then map ("- " ++) $ paths'''
                       else ("+ " ++ head paths'''):(map ("- " ++) $ tail paths''')
      return $ B8.unlines $ groupHeader t n (length paths) : map B8.pack paths''''

    nSkip ds = B8.pack $ show $ if keepOne then ds - 1 else ds

    exclude ds = if not keepOne then "exclude" else
                   if ds > 2 then "exclude all but one of"
                     else "exclude one of"

    plural :: Int -> B8.ByteString -> B8.ByteString
    plural n thing = if n > 1 then thing `B8.append` "s" else thing

    -- TODO don't mention inodes unless it's a dir, so separate fn for that
    explain :: Int -> Int -> B8.ByteString -> B8.ByteString
    explain nSaved nThings thing = B8.intercalate " "
      [ "#", exclude nThings , "these", B8.pack $ show nThings , "duplicate"
      , thing `B8.append` "s," , "saving", B8.pack $ show nSaved
      , (plural nSaved "inode") `B8.append` ":"
      ]

    -- TODO is n the number *saved*, or total number of dupes?
    groupHeader :: TreeType -> Int -> Int -> B8.ByteString
    groupHeader E _ _  = "" -- TODO is that a good idea?
    groupHeader D nSaved nDirs  = explain nSaved nDirs "folder"
    groupHeader F nSaved nFiles = explain nSaved nFiles "file"
    groupHeader _ nSaved nLinks = explain nSaved nLinks "link"
