{-# LANGUAGE OverloadedStrings #-}

module Cmd.Dupes.Render.DedupScript where

import Cmd.Dupes.Render.Types
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import Data.Word (Word8)
import System.Directory.BigTrees
import System.OsPath (OsPath, decodeFS, joinPath, splitDirectories, (</>))

fileHeader :: Bool -> B8.ByteString
fileHeader keepOne =
  "#!/usr/bin/env bash\n\
  \\n\
  \# This is the 'dedup-script' output format.\n\
  \# Be careful with this! Don't just run it without at least skimming...\n\
  \\n\
  \# You can comment, uncomment, or delete lines in your text editor\n\
  \# to change how specific files/dirs/links are handled.\n\
  \\n"
  <> (if keepOne then
  "# For each set of dupes, it will leave the first (commented out) one alone and\n\
  \# delete all the others in place by default.\n"
  else
  "# !!! WARNING !!!\n\
  \# Since you're deduping vs a reference set, this script will delete ALL dupes\n\
  \# listed below. The assumption is that you already have another copy saved\n\
  \# somewhere else, and that copy was used to generate the reference set.\n")
  <>
  "\n\
  \skip_group=FALSE\n\
  \verify() { [[ -e \"$1\" ]] && echo \"KEEP '$1'\" && skip_group=FALSE || { echo \"MISSING '$1'\" >&2; skip_group=TRUE; }; }\n\
  \skip() { [[ $skip_group == TRUE || ! -e \"$1\" ]] && echo \"SKIP '$1'\"; }\n\
  \rm_X() { skip \"$3\" || { rm $1 \"$3\" && echo \"OK $2 '$3'\"; } || { echo \"ERROR $2 '$3'\" >&2; return $?; }; }\n\
  \rm_d() { rm_X '-r' 'dir ' \"$1\"; }\n\
  \rm_f() { rm_X '' 'file' \"$1\"; }\n\
  \rm_l() { rm_X '' 'link' \"$1\"; }\n"

escapePathByte :: Char -> B8.ByteString
escapePathByte b
  | b == '\'' = B8.pack "'\\''" -- gotcha: single quote inside single-quoted path
  | otherwise = B8.singleton b

escapePath :: B8.ByteString -> B8.ByteString
escapePath path = B8.concatMap escapePathByte path

quotePath :: B8.ByteString -> B8.ByteString
quotePath path =  B8.singleton '\'' <> escapePath path <> B8.singleton '\''

addRmCall :: TreeType -> B8.ByteString -> B8.ByteString
addRmCall tt path = rm tt <> " " <> path
  where
    rm D = "rm_d"
    rm F = "rm_f"
    rm L = "rm_l" -- TODO need anything to guard against deleting target?
    rm B = "rm_l"
    rm _ = error $ "unexpected tree type " ++ show tt ++ " in path " ++ B8.unpack path

-- convert an rm_X call to a verify call
verifyRatherThanRm :: B8.ByteString -> B8.ByteString
verifyRatherThanRm rmCall = "verify" <> B8.drop 4 (rmCall)

renderDedupScript :: DupesRenderFn
renderDedupScript lCfg keepOne md ls = do
  body <- mapM excludeLines ls
  return $ fileHeader keepOne : body
  where

    depthWarning Nothing  = ""
    depthWarning (Just (Depth d)) =
      " (up to " `B8.append` B8.pack (show d) `B8.append` " levels deep)"

    excludeLines :: DupeList -> IO B8.ByteString
    excludeLines (n, h, t, paths) = do
      let paths'  = map (addRmCall t . quotePath . op2bs . snd) $ sortPaths lCfg paths
          paths'' = if keepOne
                       then (verifyRatherThanRm $ head paths') : tail paths'
                       else paths'
      return $ B8.unlines
             $ groupHeader h t n (length paths)
             : paths''

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
