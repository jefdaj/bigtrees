{-# LANGUAGE BangPatterns #-}

module System.Directory.BigTrees.HashTree.Build where

import Control.Exception.Safe (Exception, MonadCatch, handleAny)
-- import Control.Exception -- TODO specifics
-- import GHC.IO.Exception -- TODO specifics
import qualified Control.Monad.Parallel as P
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (sortBy)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Foreign.C.Types (CTime (..))
import System.Directory (doesPathExist, getFileSize, getModificationTime, pathIsSymbolicLink)
import System.Directory.BigTrees.Hash (hashFile, hashSymlinkLiteral, hashSymlinkTarget)
import System.Directory.BigTrees.HashLine (ErrMsg (..), ModTime (..), NBytes (..))
import System.Directory.BigTrees.HashTree.Base (HashTree (..), NodeData (..), ProdTree,
                                                hashContents, sumNodes, treeModTime, treeNBytes,
                                                treeName)
import System.Directory.BigTrees.Name
import qualified System.Directory.Tree as DT
import System.FilePath (takeDirectory, (</>))
import System.FilePath.Glob (CompOptions (..), MatchOptions (..), Pattern, compDefault, compileWith,
                             matchWith)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Posix.Files (getFileStatus, isDirectory, readSymbolicLink)
import System.PosixCompat.Files (fileSize, getSymbolicLinkStatus, modificationTime)

keepPath :: [String] -> FilePath -> Bool
keepPath excludes path = not $ any ((\ptn -> matchWith mOpts ptn path) . compileWith cOpts) excludes
  where
    cOpts = compDefault
              { recursiveWildcards  = True -- allow ** style globs
              -- these two together should make it match '/' literally:
              , pathSepInRanges     = False
              , errorRecovery       = True
              }
    mOpts = MatchOptions
             { matchDotsImplicitly = True
             , ignoreCase          = False
             , ignoreDotSlash      = True
             }

-- TODO hey is this not that hard to swap out for my new version?
excludeGlobs :: [String]
             -> (DT.AnchoredDirTree Name a -> DT.AnchoredDirTree Name a)
excludeGlobs excludes (a DT.:/ tree) = a DT.:/ DT.filterDir (keep a) tree
  where
    keep a (DT.Failed n _) = keepPath excludes $ DT.nappend a n
    keep a (DT.Dir  n _)   = keepPath excludes $ DT.nappend a n
    keep a (DT.File n _)   = keepPath excludes $ DT.nappend a n
    keep a b               = True

-- see also `buildTestTree` in the `HashTreeTest` module
-- TODO remove this?
-- TODO rename buildProdTreeL?
buildProdTree :: Bool -> [String] -> FilePath -> IO ProdTree
buildProdTree = buildTree (return . const ())

-- TODO rename buildTreeL?
buildTree :: (FilePath -> IO a) -> Bool -> [String] -> FilePath -> IO (HashTree a)
buildTree readFileFn beVerbose excludes path = do
  -- putStrLn $ "buildTree path: '" ++ path ++ "'"
  -- TODO attempt building lazily only to a certain depth... 10?
  -- tree <- DT.readDirectoryWithLD 10 return path -- TODO need to rename root here?
  tree <- DT.readDirectoryWithL False readFileFn path -- TODO need to rename root here?
  -- putStrLn $ show tree
  buildTree' readFileFn beVerbose 0 excludes tree

-- This is mainly meant as an error handler, but also works for the trivial
-- case of re-wrapping directory-tree error nodes.
mkErrTree :: (Monad m, Exception e) => Name -> e -> m (HashTree a)
mkErrTree n e =
  return $ Err
    { errName = n
    , errMsg = ErrMsg $ show e -- TODO clean it up a bit more?
    }

-- Note that all the IO operations done on a node itself (everything except dir
-- contents) should be strict, because we want to be able to immediately wrap
-- any IO errors in an Err tree constructor.
-- TODO is there a safer way to do that with lazy evaluation?
buildTree' :: (FilePath -> IO a) -> Bool -> Int -> [String] -> DT.AnchoredDirTree Name a -> IO (HashTree a)

buildTree' _ _ _ _  (a DT.:/ (DT.Failed n e )) = mkErrTree n e

-- A "File" can be a real file, but also several variants of symlink.
-- We handle them all here.
-- Note that readFileFn and hashFile both read the file, but in practice that
-- isn't a problem because readFileFn is a no-op in production.
buildTree' readFileFn v depth es (a DT.:/ (DT.File n _)) = handleAny (mkErrTree n) $ do
  let fPath = DT.nappend a n
  isLink <- pathIsSymbolicLink fPath -- TODO error if doesn't exist here?
  if isLink
    then do
      notBroken <- doesPathExist fPath
      -- TODO why is the if/else needed? shouldn't it short-circuit below anyway?
      notDir <- if not notBroken then return False
                else not . isDirectory <$> getFileStatus fPath
      if notBroken && notDir -- we treat links to dirs as broken for now

        then do
          -- non-broken symlink, so
          -- the symlink target is the relevant file for most data,
          -- except the mod time which should be the more recent of the two
          -- (in case the link target changed to a different valid file)
          -- TODO handle the extra case here where it exists but is outside the tree!
          !mt1 <- unsafeInterleaveIO $ getSymlinkLiteralModTime fPath
          !mt2 <- unsafeInterleaveIO $ getSymlinkTargetModTime  fPath
          let mt = max mt1 mt2
          !s  <- unsafeInterleaveIO $ getSymlinkTargetNBytes fPath
          !h  <- unsafeInterleaveIO $ hashSymlinkTarget fPath
          !fd <- unsafeInterleaveIO $ readFileFn fPath
          return $ Link
            { nodeData = NodeData
              { name = n
              , hash = h
              , modTime = mt
              , nBytes = s
              }
            , linkData = Just fd
            }

        else do
          -- broken symlink, so
          -- the symlink itself is the relevant file to pull info from
          !mt <- unsafeInterleaveIO $ getSymlinkLiteralModTime fPath
          !s  <- unsafeInterleaveIO $ getSymlinkLiteralNBytes  fPath
          !h  <- unsafeInterleaveIO $ hashSymlinkLiteral fPath
          return $ Link
            { nodeData = NodeData
              { name = n
              , hash = h
              , modTime = mt
              , nBytes = s
              }
            , linkData = Nothing
            }

    else do
      -- regular file
      !mt <- unsafeInterleaveIO $ getFileDirModTime fPath
      !s  <- unsafeInterleaveIO $ getFileDirNBytes fPath
      !h  <- unsafeInterleaveIO $ hashFile v fPath
      !fd <- unsafeInterleaveIO $ readFileFn fPath
      -- seems not to help with memory usage?
      -- return $ (\x -> hash x `seq` name x `seq` x) $ File { name = n, hash = h }
      -- return File { name = n, hash = h }
      return $ File
        { nodeData = NodeData
          { name = n
          , hash = h
          , modTime = mt
          , nBytes = s
          }
        , fileData = fd
        }

buildTree' readFileFn v depth es d@(a DT.:/ (DT.Dir n _)) = handleAny (mkErrTree n) $ do
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
  let cs'' = sortBy (compare `on` treeName) subTrees
      -- csByH = sortBy (compare `on` hash) subTrees -- no memory difference

  -- We want the overall mod time to be the most recent of the dir + all dirContents.
  -- If there are any dirContents at all, by definition they're newer than the dir, right?
  -- So we only need this root mod time when the dir is empty.
  !mt <- getFileDirModTime root
  !s  <- getFileDirNBytes root

  return $ Dir
            { dirContents = cs''
            , nNodes  = sum $ 1 : map sumNodes cs''
            , nodeData = NodeData
              { name     = n
              , modTime  = maximum $ mt : map treeModTime cs''
              , nBytes   = sum $ s : map treeNBytes cs''
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
getSymlinkTargetModTime p = do
  target <- readSymbolicLink p
  let p' = takeDirectory p </> target
  getFileDirModTime p'

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
getSymlinkTargetNBytes p = do
  target <- readSymbolicLink p
  let p' = takeDirectory p </> target
  getFileDirNBytes p'

-- Size of a regular file or directory (not including directory contents, of course)
getFileDirNBytes :: FilePath -> IO NBytes
getFileDirNBytes p = getFileSize p <&> NBytes
  -- isLink <- pathIsSymbolicLink f
  -- n <- if isLink
  --        then getSymbolicLinkStatus f >>= return . toInteger . fileSize
  --        else getFileSize f
  -- return $ NBytes n

-- TODO unit_symlink_to_dir_read_as_file
-- TODO unit_symlink_to_file_read_as_file_hash_path
-- TODO unit_symlink_to_annex_read_as_file_hash_content
