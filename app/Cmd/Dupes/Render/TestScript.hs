{-# LANGUAGE OverloadedStrings #-}

module Cmd.Dupes.Render.TestScript where

import Cmd.Dupes.Render.Types
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import System.Directory.BigTrees
import System.OsPath (OsPath, decodeFS, joinPath, splitDirectories, (</>))

fileHeader :: B8.ByteString
fileHeader = B8.pack "#!/usr/bin/env bash\n\
  \\n\
  \# This is the 'test-script' output format.\n\
  \# It's mainly for debugging cross-filesystem filename issues.\n\
  \\n\
  \test_X() { test $1 \"$3\" && echo \"OK $2 '$3'\" || { echo \"ERROR $2 '$3'\" >&2; return $?; }; }\n\
  \test_d() { test_X '-d' 'dir ' \"$1\"; }\n\
  \test_f() { test_X '-f' 'file' \"$1\"; }\n\
  \test_l() { test_X '-L' 'link' \"$1\"; }\n"

escapePathByte :: Char -> B8.ByteString
escapePathByte b
  -- | b == '\\' = B8.pack "\\\\"
  | b == '\'' = B8.pack "'\\''" -- gotcha: single quote inside single-quoted path
  | otherwise = B8.singleton b

escapePath :: B8.ByteString -> B8.ByteString
escapePath path = B8.concatMap escapePathByte path

quotePath :: B8.ByteString -> B8.ByteString
quotePath path =  B8.singleton '\'' <> escapePath path <> B8.singleton '\''

addTest :: TreeType -> B8.ByteString -> B8.ByteString
addTest tt path = test tt <> " " <> path
  where
    test D = "test_d"
    test F = "test_f"
    test l = "test_l"
    test _ = error $ "unexpected tree type " ++ show tt ++ " in path " ++ B8.unpack path

renderTestScript :: DupesRenderFn
renderTestScript keepOne md ls = do
  body <- mapM excludeLines ls
  return $ B8.unlines $ fileHeader : body
  where

    depthWarning Nothing  = ""
    depthWarning (Just (Depth d)) =
      " (up to " `B8.append` B8.pack (show d) `B8.append` " levels deep)"

    excludeLines :: DupeList -> IO B8.ByteString
    excludeLines (n, h, t, paths) = do
      return $ B8.unlines
             $ groupHeader h t n (length paths)
             : map (addTest t . quotePath . op2bs) (sortPaths paths)

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
