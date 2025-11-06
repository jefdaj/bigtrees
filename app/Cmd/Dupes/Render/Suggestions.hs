{-# LANGUAGE OverloadedStrings #-}

module Cmd.Dupes.Render.Suggestions where

import Cmd.Dupes.Render.Types
import qualified Data.ByteString.Char8 as B8
import System.Directory.BigTrees
import System.OsPath (OsPath, decodeFS, joinPath, splitDirectories, (</>))

renderSuggestions :: DupesRenderFn
renderSuggestions lCfg keepOne md ls = do
  body <- mapM excludeLines ls
  return $ B8.unlines $ fileHeader : body
  where

    fileHeader = B8.pack $
      "# This is the default 'suggestions' output format.\n\
      \# It just suggests what you might delete manually yourself.\n"
      ++ if keepOne then "" else
      "\n\
      \# Since you're deduping vs a reference set, the suggestion is to\n\
      \# delete ALL these dupes, assuming you have another copy wherever you got\n\
      \# the reference set from.\n"

    depthWarning Nothing  = ""
    depthWarning (Just (Depth d)) =
      " (up to " `B8.append` B8.pack (show d) `B8.append` " levels deep)"

    excludeLines :: DupeList -> IO B8.ByteString
    excludeLines (n, h, t, paths) = do
      return $ B8.unlines
             $ groupHeader h t n (length paths)
             : map (op2bs . snd) (sortPaths lCfg paths)

    groupHeader :: Hash -> TreeType -> Int -> Int -> B8.ByteString
    groupHeader _ E _ _ = "" -- TODO is that a good idea?
    groupHeader h D nSaved nDirs = B8.intercalate " "
      [ "# You could save", B8.pack (show nSaved)
      , "inodes by deleting "
          <> (if keepOne then "all but one of " else mempty)
          <> "these", B8.pack (show nDirs)
      , "duplicate directories with hash", prettyHash h `B8.append` (depthWarning md)
      ]
    groupHeader h F nSaved nFiles = B8.intercalate " "
      [ "# You could delete", B8.pack (show $ nFiles - 1)
      , "of these", B8.pack (show nFiles)
      , "duplicate files with hash", prettyHash h `B8.append` (depthWarning md)
      ]
    groupHeader h _ nSaved nLinks = B8.intercalate " "
      [ "# You could delete", B8.pack (show $ nLinks - 1)
      , "of these"  , B8.pack (show nLinks)
      , "duplicate links with hash", prettyHash h `B8.append` (depthWarning md)
      ]
