{-# LANGUAGE BangPatterns #-}

module System.Directory.BigTrees.HashTree.Build where

import qualified Control.Monad.Parallel as P
import Data.Function (on)
import Data.List (sortBy)
import System.Directory.BigTrees.Hash (hashFile, hashSymlinkTarget, hashSymlinkLiteral)
import System.Directory.BigTrees.HashLine (ModTime(..), NBytes(..))
import System.Directory.BigTrees.HashTree.Base (HashTree (..), NodeData(..), ProdTree, sumNodes, hashContents)
import System.Directory.BigTrees.Name
import System.Directory (getFileSize, getModificationTime, pathIsSymbolicLink, doesPathExist)
import qualified System.Directory.Tree as DT
import System.FilePath ((</>))
import System.FilePath.Glob (MatchOptions (..), Pattern, matchWith, compile)
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.PosixCompat.Files (getSymbolicLinkStatus, modificationTime, fileSize)
import Foreign.C.Types (CTime(..))
import System.Posix.Files (readSymbolicLink)

keepPath :: [String] -> FilePath -> Bool
keepPath excludes path = not $ any (\ptn -> matchWith opts ptn path) (map compile excludes)
  where
    opts = MatchOptions
             { matchDotsImplicitly = True
             , ignoreCase          = False
             , ignoreDotSlash      = True
             }

-- TODO hey is this not that hard to swap out for my new version?
excludeGlobs :: [String]
             -> (DT.AnchoredDirTree Name a -> DT.AnchoredDirTree Name a)
excludeGlobs excludes (a DT.:/ tree) = a DT.:/ DT.filterDir (keep a) tree
  where
    keep a (DT.Dir  n _) = keepPath excludes $ DT.nappend a n
    keep a (DT.File n _) = keepPath excludes $ DT.nappend a n
    keep a b             = True

-- TODO take this as a command-line argument?
lazyDirDepth :: Int
lazyDirDepth = 4

-- see also `buildTestTree` in the `HashTreeTest` module
-- TODO remove this?
buildProdTree :: Bool -> [String] -> FilePath -> IO ProdTree
buildProdTree = buildTree (return . const ())

buildTree :: (FilePath -> IO a) -> Bool -> [String] -> FilePath -> IO (HashTree a)
buildTree readFileFn beVerbose excludes path = do
  -- putStrLn $ "buildTree path: '" ++ path ++ "'"
  -- TODO attempt building lazily only to a certain depth... 10?
  -- tree <- DT.readDirectoryWithLD 10 return path -- TODO need to rename root here?
  tree <- DT.readDirectoryWithL False readFileFn path -- TODO need to rename root here?
  -- putStrLn $ show tree
  buildTree' readFileFn beVerbose 0 excludes tree

buildTree' :: (FilePath -> IO a) -> Bool -> Int -> [String] -> DT.AnchoredDirTree Name a -> IO (HashTree a)

-- TODO catch and re-throw errors with better description and/or handle them here
buildTree' _ _ _ _  (a DT.:/ (DT.Failed n e )) = error $ DT.nappend a n ++ ": " ++ show e

-- A "File" can be a real file, but also several variants of symlink.
-- We handle them all here.
-- Note that readFileFn and hashFile both read the file, but in practice that
-- isn't a problem because readFileFn is a no-op in production.
buildTree' readFileFn v depth es (a DT.:/ (DT.File n _)) = do
  let fPath = DT.nappend a n
  isLink <- pathIsSymbolicLink fPath
  if isLink
    then do
      notBroken <- doesPathExist fPath
      if notBroken

        then do
          -- the symlink target is the relevant file for most data,
          -- except the mod time which should be the more recent of the two
          -- (in case the link target changed to a different valid file)
          -- TODO handle the extra case here where it exists but is outside the tree!
          !mt1 <- getSymlinkLiteralModTime fPath -- TODO interleave?
          !mt2 <- getSymlinkTargetModTime  fPath -- TODO interleave?
          let mt = maximum [mt1, mt2]
          !s  <- getSymlinkTargetNBytes fPath -- TODO interleave?
          !h  <- unsafeInterleaveIO $ hashSymlinkTarget fPath
          !fd <- unsafeInterleaveIO $ readFileFn fPath
          return $ (if depth < lazyDirDepth
                      then id
                      else (\x -> nodeData x `seq` x)) -- TODO what else needs to be here??
                 $ File -- TODO Link
                    { nodeData = NodeData
                      { name = n
                      , hash = h
                      , modTime = mt
                      , nBytes = s
                      }
                    , fileData = fd
                    }


        else do
          -- the symlink itself is the relevant file to pull info from
          !mt <- getSymlinkLiteralModTime fPath -- TODO interleave?
          !s  <- getSymlinkLiteralNBytes  fPath -- TODO interleave?
          !h  <- unsafeInterleaveIO $ hashSymlinkLiteral fPath
          !fd <- unsafeInterleaveIO $ undefined fPath -- TODO should be Nothing in the Link here
          return $ (if depth < lazyDirDepth
                      then id
                      else (\x -> nodeData x `seq` x)) -- TODO what else needs to be here??
                 $ File -- TODO Link
                    { nodeData = NodeData
                      { name = n
                      , hash = h
                      , modTime = mt
                      , nBytes = s
                      }
                    , fileData = fd
                    }


    else do
      -- regular file
      !mt <- getFileDirModTime fPath -- TODO interleave?
      !s  <- getFileDirNBytes fPath -- TODO interleave?
      !h  <- unsafeInterleaveIO $ hashFile v fPath
      !fd <- unsafeInterleaveIO $ readFileFn fPath
      -- seems not to help with memory usage?
      -- return $ (\x -> hash x `seq` name x `seq` x) $ File { name = n, hash = h }
      -- return File { name = n, hash = h }
      return $ (if depth < lazyDirDepth
                  then id
                  else (\x -> nodeData x `seq` x)) -- TODO what else needs to be here??
             $ File
                { nodeData = NodeData
                  { name = n
                  , hash = h
                  , modTime = mt
                  , nBytes = s
                  }
                , fileData = fd
                }

buildTree' readFileFn v depth es d@(a DT.:/ (DT.Dir n _)) = do
  let root = DT.nappend a n
      -- bang t has no effect on memory usage
      hashSubtree t = unsafeInterleaveIO $ buildTree' readFileFn v (depth+1) es $ root DT.:/ t
      (_ DT.:/ (DT.Dir _ cs')) = excludeGlobs es d -- TODO operate on only the cs part

  -- this works, but doesn't affect memory usage:
  -- subTrees <- (if depth > 10 then M.forM else P.forM) cs' hashSubtree

  subTrees <- P.forM cs' hashSubtree

  -- sorting by hash is better in that it catches file renames,
  -- but sorting by name is better in that it lets you stream hashes to stdout.
  -- so we do both: name when building the tree, then hash when computing dir hashes
  let cs'' = sortBy (compare `on` (name . nodeData)) subTrees
      -- csByH = sortBy (compare `on` hash) subTrees -- no memory difference

  -- We want the overall mod time to be the most recent of the dir + all dirContents.
  -- If there are any dirContents at all, by definition they're newer than the dir, right?
  -- So we only need the root mod time when the dir is empty...
  -- !mt <- getFileDirModTime root
  mt <- if null cs''
          then getFileDirModTime root
          else return $ maximum $ map (modTime . nodeData) cs''

  !s  <- getFileDirNBytes root -- TODO is this always 4096?

  -- use lazy evaluation up to 5 levels deep, then strict
  -- TODO should that be configurable or something?
  return $ (if depth < lazyDirDepth
              then id
              else (\r -> (nodeData r `seq` nNodes r) `seq` r)) -- TODO what else needs to be here??
         $ Dir
            { dirContents = cs''
            , nNodes  = sum $ 1 : map sumNodes cs''
            , nodeData = NodeData
              { name     = n
              , modTime  = mt
              , nBytes   = sum $ s : map (nBytes . nodeData) cs''
              , hash     = hashContents cs''
              }
            }

-- TODO move these to Util:

-- Mod time of a symlink itself (not the target)
getSymlinkLiteralModTime :: FilePath -> IO ModTime
getSymlinkLiteralModTime p = do
  (CTime s) <- modificationTime <$> getSymbolicLinkStatus p
  return $ ModTime $ toInteger s

-- Mod time of a symlink target, if it exists
-- TODO fix this so it works recursively in case of more than one link!
getSymlinkTargetModTime :: FilePath -> IO ModTime
getSymlinkTargetModTime p = readSymbolicLink p >>= getFileDirModTime

-- https://stackoverflow.com/a/17909816
-- Be sure to check that it isn't a symlink before calling this!
-- It works on directories as well as regular files, but only updates dirs if
-- the dir itself changes, *not* if the contents change.
-- TODO if git reports file is older than mod time does, trust git?
-- TODO going to have to update dirs recursively based on newest content change
getFileDirModTime :: FilePath -> IO ModTime
getFileDirModTime f = do
  s <- getModificationTime f
  return $ ModTime $ toInteger $ round $ utcTimeToPOSIXSeconds s

getSymlinkLiteralNBytes :: FilePath -> IO NBytes
getSymlinkLiteralNBytes p = do
  status <- getSymbolicLinkStatus p
  return $ NBytes $ toInteger $ fileSize status

-- TODO does this work recursively?
-- TODO is it ever needed?
getSymlinkTargetNBytes :: FilePath -> IO NBytes
getSymlinkTargetNBytes p = readSymbolicLink p >>= getFileDirNBytes

-- Size of a regular file or directory (not including directory contents, of course)
getFileDirNBytes :: FilePath -> IO NBytes
getFileDirNBytes f = do
  isLink <- pathIsSymbolicLink f
  n <- if isLink
         then getSymbolicLinkStatus f >>= return . toInteger . fileSize
         else getFileSize f
  return $ NBytes n

-- TODO unit_symlink_to_dir_read_as_file
-- TODO unit_symlink_to_file_read_as_file_hash_path
-- TODO unit_symlink_to_annex_read_as_file_hash_content
