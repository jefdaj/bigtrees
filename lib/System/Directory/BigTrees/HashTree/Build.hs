{-# LANGUAGE BangPatterns #-}

module System.Directory.BigTrees.HashTree.Build where

import qualified Control.Monad.Parallel as P
import Data.Function (on)
import Data.List (sortBy)
import System.Directory.BigTrees.Hash (hashFile)
import System.Directory.BigTrees.HashLine (ModTime(..), Size(..))
import System.Directory.BigTrees.HashTree.Base (HashTree (..), ProdTree, totalNodes, hashContents)
import System.Directory.BigTrees.Name
import System.Directory (getFileSize, getModificationTime)
import qualified System.Directory.Tree as DT
import System.FilePath ((</>))
import System.FilePath.Glob (MatchOptions (..), Pattern, matchWith)
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

keepPath :: [Pattern] -> FilePath -> Bool
keepPath excludes path = not $ any (\ptn -> matchWith opts ptn path) excludes
  where
    opts = MatchOptions
             { matchDotsImplicitly = True
             , ignoreCase          = False
             , ignoreDotSlash      = True
             }

-- TODO hey is this not that hard to swap out for my new version?
excludeGlobs :: [Pattern]
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
buildProdTree :: Bool -> [Pattern] -> FilePath -> IO ProdTree
buildProdTree = buildTree (return . const ())

-- TODO are contents sorted? they probably should be for stable hashes
buildTree :: (FilePath -> IO a) -> Bool -> [Pattern] -> FilePath -> IO (HashTree a)
buildTree readFileFn beVerbose excludes path = do
  -- putStrLn $ "buildTree path: '" ++ path ++ "'"
  -- TODO attempt building lazily only to a certain depth... 10?
  -- tree <- DT.readDirectoryWithLD 10 return path -- TODO need to rename root here?
  tree <- DT.readDirectoryWithL readFileFn path -- TODO need to rename root here?
  -- putStrLn $ show tree
  buildTree' readFileFn beVerbose 0 excludes tree

-- TODO oh no, does AnchoredDirTree fail on cyclic symlinks?
buildTree' :: (FilePath -> IO a) -> Bool -> Int -> [Pattern] -> DT.AnchoredDirTree Name a -> IO (HashTree a)
-- TODO catch and re-throw errors with better description and/or handle them here
buildTree' _ _ _ _  (a DT.:/ (DT.Failed n e )) = error $ DT.nappend a n ++ ": " ++ show e
buildTree' readFileFn v depth es (a DT.:/ (DT.File n _)) = do
  -- TODO how to exclude these?
  let fPath = DT.nappend a n
  -- TODO hold up, are we reading the file twice here?
  --      oh right: not usually a problem because readFileFn is a no-op in production
  !mt <- getModTime fPath
  !s  <- getSize fPath
  !h  <- unsafeInterleaveIO $ hashFile v fPath -- TODO symlink bug here?
  !fd <- unsafeInterleaveIO $ readFileFn fPath -- TODO is this safe enough?
  -- seems not to help with memory usage?
  -- return $ (\x -> hash x `seq` name x `seq` x) $ File { name = n, hash = h }
  -- return File { name = n, hash = h }
  return $ (if depth < lazyDirDepth
              then id
              else (\x -> hash x `seq` name x `seq` x))
         $ File { name = n, hash = h, modTime = mt, size = s, fileData = fd }

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
  let cs'' = sortBy (compare `on` name) subTrees
      -- csByH = sortBy (compare `on` hash) subTrees -- no memory difference

  -- We want the overall mod time to be the most recent of the dir + all contents.
  -- If there are any contents at all, by definition they're newer than the dir, right?
  -- So we only need the root mod time when the dir is empty...
  -- !mt <- getModTime root
  mt <- if null cs''
          then getModTime root
          else return $ maximum $ map modTime cs''

  !s  <- getSize root -- TODO is this always 4096?

  -- use lazy evaluation up to 5 levels deep, then strict
  -- TODO should that be configurable or something?
  return $ (if depth < lazyDirDepth
              then id
              else (\r -> (hash r `seq` nNodes r `seq` hash r) `seq` r)) -- TODO also mt?
         $ Dir
            { name     = n
            , contents = cs''
            , modTime  = mt
            , size     = sum $ s : map size cs''
            , hash     = hashContents cs''
            , nNodes  = sum $ 1 : map totalNodes cs''
            }

-- https://stackoverflow.com/a/17909816
-- TODO if git reports file is older than mod time does, trust git?
-- TODO going to have to update dirs recursively based on newest content change
getModTime :: FilePath -> IO ModTime
getModTime f = do
  mt <- getModificationTime f
  let sec = round $ utcTimeToPOSIXSeconds mt
  return $ ModTime sec

-- Size in bytes
getSize :: FilePath -> IO Size
getSize f = Size <$> getFileSize f
