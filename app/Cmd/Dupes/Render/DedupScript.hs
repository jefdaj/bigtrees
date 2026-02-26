{-# LANGUAGE OverloadedStrings #-}

module Cmd.Dupes.Render.DedupScript where

import Cmd.Dupes.Render.Types
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import Data.Word (Word8)
import System.Directory.BigTrees
import System.OsPath (OsPath, decodeFS, joinPath, splitDirectories, (</>))

-- TODO flag for whether to delete last copy, and set FALSE when keepOne

fileHeader :: Bool -> B8.ByteString
fileHeader keepOne =
  "#!/usr/bin/env bash\n\
  \\n\
  \# This is the dedup-script output format.\n\
  \# Be careful with this! Don't just run it without at least skimming...\n\
  \\n\
  \# You can comment or delete lines in your text editor\n\
  \# to change how specific files/dirs/links are handled.\n\
  \\n"
  <> (if keepOne then
  "# For each set of dupes, confirm that one copy exists and\n\
  \# then delete all the others. You probably don't want to change this.\n\
  \KEEP_ONE=TRUE\n"
  else
  "# !!! WARNING !!!\n\
  \# Since you're deduping vs a reference set, this script will delete ALL dupes\n\
  \# listed below. The assumption is that you already have another copy saved\n\
  \# somewhere else, and that copy was used to generate the reference set.\n\
  \KEEP_ONE=FALSE\n")
  <>
  "\n\
  \# Set this to TRUE to print what would be deleted rather than actually deleting it.\n\
  \DRY_RUN=FALSE\n\
  \\n\
  \set -euo pipefail\n\
  \keeper=\"\" set_hash=\"\" set_type=\"\" set_total=0 set_skipped=0 set_removed=0 n_removed=0 n_errors=0\n\
  \ dupe_set() {\n\
  \ if [[ \"$set_total\" > 0 ]]; then\n\
  \   echo -n \"$set_hash $set_total $set_type: skip $set_skipped, rm $set_removed\"\n\
  \   if [[ -z \"$keeper\" ]]; then\n\
  \     echo \", keep 0\"\n\
  \   else\n\
  \     echo \", keep '$keeper'\"\n\
  \   fi\n\
  \ fi\n\
  \ keeper=\"\"; set_hash=\"$1\"; set_type=\"$2\"; set_total=0; set_skipped=0; set_removed=0\n\
  \}\n\
  \dupe() {\n\
  \  ((set_total++)) ||:\n\
  \  if [[ ! -e \"$1\" ]]; then\n\
  \    ((set_skipped++)) ||:;\n\
  \  elif [[ $KEEP_ONE != FALSE && -z \"$keeper\" ]]; then\n\
  \    keeper=\"$1\"\n\
  \  elif [[ $DRY_RUN != FALSE ]] || rm -r \"$1\" 2>/dev/null; then\n\
  \    ((set_removed++)) ||:; ((n_removed++)) ||:\n\
  \    [[ $DRY_RUN != FALSE ]] && echo \"rm -r '$1'\"\n\
  \  else\n\
  \    ((n_errors++)) ||:; echo \"  ERROR removing: $1\" >&2\n\
  \  fi\n\
  \}\n\
  \trap 'dupe_set \"\" \"\"; echo; echo \"Total: $n_removed removed, $n_errors errors\"' EXIT\n"

escapePathByte :: Char -> B8.ByteString
escapePathByte b
  | b == '\'' = B8.pack "'\\''" -- gotcha: single quote inside single-quoted path
  | otherwise = B8.singleton b

escapePath :: B8.ByteString -> B8.ByteString
escapePath path = B8.concatMap escapePathByte path

quotePath :: B8.ByteString -> B8.ByteString
quotePath path =  B8.singleton '\'' <> escapePath path <> B8.singleton '\''

addDupeCall :: TreeType -> B8.ByteString -> B8.ByteString
addDupeCall tt path = rm tt <> " " <> path
  where
    rm D = "dupe"
    rm F = "dupe"
    rm L = "dupe" -- TODO need anything to guard against deleting target?
    rm B = "dupe"
    rm _ = error $ "unexpected tree type " ++ show tt ++ " in path " ++ B8.unpack path

-- Dirs before all others is important for dedup-scripts because if the larger
-- sets of duplicate files come before any directories, it's possible that this
-- will happen:
-- 1. all but one of a set of file dupes is deleted
-- 2. all but one of a set of dir dupes is deleted,
--    and the one remaining copy of the file was in one of the deleted dirs!
-- Larger dirs before smaller ones is also important for similar reasons.
-- TODO explain that part better
sortForSafeDedup :: SortedDupeLists -> SortedDupeLists
sortForSafeDedup lists = largestFirst dirs ++ nonDirs
  where
    (dirs, nonDirs) = L.partition (\(_,_,t,_) -> t == D) lists
    largestFirst    = L.sortOn $ \(n, _, _, _) -> n

renderDedupScript :: DupesRenderFn
renderDedupScript lCfg keepOne md ls = do
  let ls' = sortForSafeDedup ls
  body <- mapM excludeLines ls'
  return $ fileHeader keepOne : body
  where

    -- TODO rewrite to work with current script?
    -- depthWarning Nothing  = ""
    -- depthWarning (Just (Depth d)) =
    --   " (up to " `B8.append` B8.pack (show d) `B8.append` " levels deep)"

    excludeLines :: DupeList -> IO B8.ByteString
    excludeLines (n, h, t, paths) = do
      let paths'  = map (addDupeCall t . quotePath . op2bs . snd) $ sortPaths lCfg paths
      return $ B8.unlines
             $ groupHeader h t n (length paths)
             : paths'

    groupHeader :: Hash -> TreeType -> Int -> Int -> B8.ByteString
    groupHeader _ E _ _ = "" -- TODO is that a good idea?
    groupHeader h D nSaved nDirs  = "dupe_set '" <> prettyHash h <> "' 'dirs'"
    groupHeader h F nSaved nFiles = "dupe_set '" <> prettyHash h <> "' 'files'"
    groupHeader h _ nSaved nLinks = "dupe_set '" <> prettyHash h <> "' 'links'"
