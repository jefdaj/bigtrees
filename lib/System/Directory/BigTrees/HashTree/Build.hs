{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes  #-}

module System.Directory.BigTrees.HashTree.Build where

import Control.Exception.Safe (Exception, MonadCatch, handleAny)
-- import Control.Exception -- TODO specifics
-- import GHC.IO.Exception -- TODO specifics
import Control.Monad (filterM, unless)
import qualified Control.Monad.Parallel as P
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (elem, intercalate, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Foreign.C.Types (CTime (..))
-- import System.Directory (doesPathExist, getFileSize, getModificationTime, pathIsSymbolicLink)
import System.Directory.BigTrees.Hash (hashFile, hashFromAnnexPath, hashSymlinkLiteral,
                                       hashSymlinkTarget)
import System.Directory.BigTrees.HashLine (Depth (..), ErrMsg (..), ModTime (..), NBytes (..),
                                           simplifyErrMsg)
import System.Directory.BigTrees.HashTree.Base (HashTree (..), NodeData (..), ProdTree,
                                                hashContents, sortContentsByName, sumNodes,
                                                treeModTime, treeNBytes, treeName)
import System.Directory.BigTrees.HashTree.Search (SearchConfig (..))
import System.Directory.BigTrees.Name
import qualified System.Directory.OsPath as SDO
import qualified System.Directory.Tree as DT
import qualified System.OsPath as SOP
import System.OsPath (OsPath, decodeFS, encodeFS, takeDirectory, (</>))
-- import System.FilePath.Glob (CompOptions (..), MatchOptions (..), Pattern, compDefault, compileWith,
--                              matchWith)
import Data.Char
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Posix.Files (getFileStatus, readSymbolicLink, isRegularFile)
import System.PosixCompat.Files (fileSize, getSymbolicLinkStatus, modificationTime)
import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString

-- import Debug.Trace

-- import qualified System.File.OsPath as SFO

-- keepPath :: [String] -> OsPath -> Bool
-- keepPath excludes path = not $ any ((\ptn -> matchWith mOpts ptn path) . compileWith cOpts) excludes
--   where
--     cOpts = compDefault
--               { recursiveWildcards  = True -- allow ** style globs
--               -- these two together should make it match '/' literally:
--               , pathSepInRanges     = False
--               , errorRecovery       = True
--               }
--     mOpts = MatchOptions
--              { matchDotsImplicitly = True
--              , ignoreCase          = False
--              , ignoreDotSlash      = True
--              }

-- TODO double check the breadcrumbs aren't backward
keepPath :: SearchConfig -> OsPath -> IO Bool
keepPath cfg p = do
  path <- decodeFS p
  return $ not $ any (path =~) (excludeRegexes cfg)

regexFilterTrees :: SearchConfig -> OsPath -> [DT.DirTree a] -> IO [DT.DirTree a]
regexFilterTrees cfg rootDir trees = filterM noExclude trees
  where
    noExclude t = keepPath cfg $ rootDir </> (DT.name t)

-- TODO hey is this not that hard to swap out for my new version?
-- TODO have this apply to the whole paths, not just one name/component?
-- excludeRegexes :: [String] -> DT.DirTree a -> IO (DT.DirTree a)
-- excludeRegexes excludes tree = DT.filterDirIO keep tree
--   where
--     keep (DT.Failed n _) = keepPath excludes n
--     keep (DT.Dir  n _)   = keepPath excludes n
--     keep (DT.File n _)   = keepPath excludes n
--     keep b               = return True

-- see also `buildTestTree` in the `HashTreeTest` module
-- TODO remove this?
-- TODO rename buildProdTreeL?
buildProdTree :: SearchConfig -> Bool -> OsPath -> IO ProdTree
buildProdTree cfg = buildTree cfg (return . const ())

-- TODO rename buildTreeL?
buildTree :: SearchConfig -> (OsPath -> IO a) -> Bool -> OsPath -> IO (HashTree a)
buildTree cfg readFileFn beVerbose path = do
  -- putStrLn $ "buildTree path: '" ++ path ++ "'"
  -- TODO attempt building lazily only to a certain depth... 10?
  -- tree <- DT.readDirectoryWithLD 10 return path -- TODO need to rename root here?
  tree <- DT.readDirectoryWithL readFileFn path -- TODO need to rename root here?
  -- putStrLn $ show tree
  buildTree' cfg readFileFn beVerbose (Depth 0) tree

-- This is mainly meant as an error handler, but also works for the trivial
-- case of re-wrapping directory-tree error nodes.
mkErrTree :: (Show e) => DT.FileName -> e -> IO (HashTree a)
mkErrTree n e = do
  return $ Err
    { errName = Name n
    , errMsg = ErrMsg $ simplifyErrMsg $ show e
    }

-- | Absolute paths are NOT "in the tree", even if they currently happen to be,
-- since that could change upon relocating the tree or the .bigtree file.
-- This is really more like "path is reliably in the tree".
-- Relative paths might be in the tree, if they don't have more .. levels than
-- the current depth.
-- TODO move to Util? Tree Base?
-- TODO Is it possible to make this technically correct in presence of more symlinks?
--      Probably not. See Niel's blog post to double check though.
pathIsInTree :: Depth -> Maybe OsPath -> Bool
pathIsInTree _ Nothing = False
pathIsInTree (Depth d) (Just path) = notAbsolute && foldHeight comps < d
  where
    notAbsolute = not $ SOP.isAbsolute path
    comps       = SOP.splitDirectories path
    dot         = [SOP.osp|.|]
    dotdot      = [SOP.osp|..|]
    foldHeight [] = 0
    foldHeight (p:ps) = case p of
                         dot    -> foldHeight ps
                         dotdot -> foldHeight ps + 1
                         _      -> foldHeight ps - 1


-- Note that all the IO operations done on a node itself (everything except dir
-- contents) should be strict, because we want to be able to immediately wrap
-- any IO errors in an Err tree constructor.
-- TODO is there a safer way to do that with lazy evaluation?
buildTree' :: SearchConfig -> (OsPath -> IO a) -> Bool -> Depth -> DT.AnchoredDirTree a -> IO (HashTree a)

buildTree' _ _ _ _  (a DT.:/ (DT.Failed n e )) = mkErrTree n e

-- A "File" can be a real file, but also several variants of symlink.
-- We handle them all here.
-- Note that readFileFn and hashFile both read the file, but in practice that
-- isn't a problem because readFileFn is a no-op in production.
buildTree' _ readFileFn v depth (a DT.:/ (DT.File n _)) = handleAny (mkErrTree n) $ do
  let fPath = a </> n
  fPath' <- SOP.decodeFS fPath
  notBroken <- SDO.doesPathExist      fPath
  isLink    <- SDO.pathIsSymbolicLink fPath
  -- TODO why is the if/else needed here?
  notDir    <- if not notBroken then return False
               else not <$> SDO.doesDirectoryExist fPath
  isRegular <- isRegularFile <$> getFileStatus fPath'
  if isLink
    then do

      !target <- SDO.getSymbolicLinkTarget fPath
      let doHashContents = notBroken && notDir && pathIsInTree depth (Just target)

      if doHashContents
        then do
          -- the symlink target is the relevant file for most data,
          -- except the mod time which should be the more recent of the two
          -- (in case the link target changed to a different valid file)
          !mt1 <- unsafeInterleaveIO $ getSymlinkLiteralModTime fPath
          !mt2 <- unsafeInterleaveIO $ getSymlinkTargetModTime  fPath
          let mt = max mt1 mt2
          !s  <- unsafeInterleaveIO $ getSymlinkTargetNBytes fPath
          !h  <- unsafeInterleaveIO $ hashSymlinkTarget fPath
          !fd <- unsafeInterleaveIO $ readFileFn fPath
          return $ Link
            { nodeData = NodeData
              { name = Name n
              , hash = h
              , modTime = mt
              , nBytes = s
              }
            , linkData = Just fd
            , linkInTree = True
            , linkTarget = target
            }

        else do
          -- "broken" symlink, including the case where it points outside the tree.
          -- either way the symlink itself is the relevant file to pull info from.
          !mt <- unsafeInterleaveIO $ getSymlinkLiteralModTime fPath
          !s  <- unsafeInterleaveIO $ getSymlinkLiteralNBytes  fPath
          !h  <- unsafeInterleaveIO $ hashSymlinkLiteral fPath
          return $ Link
            { nodeData = NodeData
              { name = Name n
              , hash = h
              , modTime = mt
              , nBytes = s
              }
            , linkData = Nothing
            , linkInTree = False
            , linkTarget = target
            }

    -- we're past all the special cases we know how to handle,
    -- so the last thing to check is whether it's one of the "weird" files
    -- that can cause hashing to freeze, like fifos or block devices
    -- TODO are there any other "non-regular" files we can do something useful with?
    else if not isRegular then mkErrTree n "not a regular file"

    else do
      -- actual regular file
      !mt <- unsafeInterleaveIO $ getFileDirModTime fPath
      !s  <- unsafeInterleaveIO $ getFileDirNBytes fPath

      -- try to get hash from annex path, or hash if needed
      tmp <- hashFromAnnexPath fPath
      !h <- case tmp of
              Just h  -> return h
              Nothing -> unsafeInterleaveIO $ hashFile v fPath

      !fd <- unsafeInterleaveIO $ readFileFn fPath
      -- seems not to help with memory usage?
      -- return $ (\x -> hash x `seq` name x `seq` x) $ File { name = n, hash = h }
      -- return File { name = n, hash = h }
      return $ File
        { nodeData = NodeData
          { name = Name n
          , hash = h
          , modTime = mt
          , nBytes = s
          }
        , fileData = fd
        }

buildTree' cfg readFileFn v depth (a DT.:/ d@(DT.Dir n cs)) = handleAny (mkErrTree n) $ do

  -- TODO of course, this is forcing the whole tree! have to be lazier about it
  -- (DT.Dir _ cs') <- excludeRegexes es d -- TODO was the idea to only operate on cs?

  -- TODO does this break lazy evaluation? or is it ok?
  -- (maybe it's handled by sorting in directory-tree anyway now?)
  -- let cs' = sortBy (compare `on` DT.name) cs
  -- TODO also do this while reading a tree, right? apply filters in a uniform way everywhere!
  cs'' <- regexFilterTrees cfg a cs
  let root = a </> n
      -- bang t has no effect on memory usage
      hashSubtree t = unsafeInterleaveIO $ buildTree' cfg readFileFn v (depth+1) $ root DT.:/ t

  -- this works, but doesn't affect memory usage:
  -- subTrees <- (if depth > 10 then M.forM else P.forM) cs' hashSubtree

  subTrees <- P.forM cs'' hashSubtree

  -- sorting by hash is better in that it catches file renames,
  -- but sorting by name is better in that it lets you stream hashes to stdout.
  -- so we do both: name when building the tree, then hash when computing dir hashes
  -- let cs'' = sortContentsByName subTrees
      -- csByH = sortBy (compare `on` hash) subTrees -- no memory difference

  -- We want the overall mod time to be the most recent of the dir + all dirContents.
  -- If there are any dirContents at all, by definition they're newer than the dir, right?
  -- So we only need this root mod time when the dir is empty.
  mt <- getFileDirModTime root
  s  <- getFileDirNBytes root

  return $ Dir
            { dirContents = subTrees
            , nNodes  = sum $ 1 : map sumNodes subTrees
            , nodeData = NodeData
              { name     = Name n
              , modTime  = maximum $ mt : map treeModTime subTrees
              , nBytes   = sum $ s : map treeNBytes subTrees
              , hash     = hashContents subTrees
              }
            }

-- TODO move these to Util:

-- Mod time of a symlink itself (not the target)
getSymlinkLiteralModTime :: OsPath -> IO ModTime
getSymlinkLiteralModTime p = do
  -- (CTime s) <- modificationTime <$> getSymbolicLinkStatus p
  s <- SDO.getModificationTime p
  return $ ModTime $ round $ utcTimeToPOSIXSeconds s

-- Mod time of a symlink target, if it exists
-- TODO fix this so it works recursively in case of more than one link!
getSymlinkTargetModTime :: OsPath -> IO ModTime
getSymlinkTargetModTime p = do
  target <- SDO.getSymbolicLinkTarget p
  let p' = takeDirectory p </> target
  getFileDirModTime p'

-- https://stackoverflow.com/a/17909816
-- Be sure to check that it isn't a symlink before calling this!
-- It works on directories as well as regular files, but only updates dirs if
-- the dir itself changes, *not* if the contents change.
-- TODO if git reports file is older than mod time does, trust git?
-- TODO going to have to update dirs recursively based on newest content change
getFileDirModTime :: OsPath -> IO ModTime
getFileDirModTime f = do
  s <- SDO.getModificationTime f
  return $ ModTime $ round $ utcTimeToPOSIXSeconds s

getSymlinkLiteralNBytes :: OsPath -> IO NBytes
getSymlinkLiteralNBytes p = do
  -- status <- getSymbolicLinkStatus p
  -- return $ NBytes $ toInteger $ fileSize status
  fsint <- SDO.getFileSize p
  return $ NBytes fsint

-- TODO does this work recursively?
-- TODO is it ever needed?
getSymlinkTargetNBytes :: OsPath -> IO NBytes
getSymlinkTargetNBytes p = do
  target <- SDO.getSymbolicLinkTarget p
  let p' = takeDirectory p </> target
  getFileDirNBytes p'

-- Size of a regular file or directory (not including directory contents, of course)
getFileDirNBytes :: OsPath -> IO NBytes
getFileDirNBytes p = SDO.getFileSize p <&> NBytes
  -- isLink <- pathIsSymbolicLink f
  -- n <- if isLink
  --        then getSymbolicLinkStatus f >>= return . toInteger . fileSize
  --        else getFileSize f
  -- return $ NBytes n

-- TODO unit_symlink_to_dir_read_as_file
-- TODO unit_symlink_to_file_read_as_file_hash_path
-- TODO unit_symlink_to_annex_read_as_file_hash_content
