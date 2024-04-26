{-# LANGUAGE BangPatterns #-}

module System.Directory.BigTrees.HashTree.Build where

import qualified Control.Monad.Parallel as P
import Data.Function (on)
import Data.List (sortBy)
import System.Directory.BigTrees.FilePath (n2fp)
import System.Directory.BigTrees.Hash (hashFile)
import System.Directory.BigTrees.HashLine ()
import System.Directory.BigTrees.HashTree.Base (HashTree (..), ProdTree, countFiles, hashContents)
import qualified System.Directory.Tree as DT
import System.FilePath ((</>))
import System.FilePath.Glob (MatchOptions (..), Pattern, matchWith)
import System.IO.Unsafe (unsafeInterleaveIO)

keepPath :: [Pattern] -> FilePath -> Bool
keepPath excludes path = not $ any (\ptn -> matchWith opts ptn path) excludes
  where
    opts = MatchOptions
             { matchDotsImplicitly = True
             , ignoreCase          = False
             , ignoreDotSlash      = True
             }

excludeGlobs :: [Pattern]
             -> (DT.AnchoredDirTree a -> DT.AnchoredDirTree a)
excludeGlobs excludes (a DT.:/ tree) = a DT.:/ DT.filterDir (keep a) tree
  where
    keep a (DT.Dir  n _) = keepPath excludes (a </> n2fp n)
    keep a (DT.File n _) = keepPath excludes (a </> n2fp n)
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
buildTree' :: (FilePath -> IO a) -> Bool -> Int -> [Pattern] -> DT.AnchoredDirTree a -> IO (HashTree a)
-- TODO catch and re-throw errors with better description and/or handle them here
buildTree' _ _ _ _  (a DT.:/ (DT.Failed n e )) = error $ (a </> n2fp n) ++ ": " ++ show e
buildTree' readFileFn v depth es (a DT.:/ (DT.File n _)) = do
  -- TODO how to exclude these?
  let fPath = a </> n2fp n
  !h  <- unsafeInterleaveIO $ hashFile v fPath
  !fd <- unsafeInterleaveIO $ readFileFn fPath -- TODO is this safe enough?
  -- seems not to help with memory usage?
  -- return $ (\x -> hash x `seq` name x `seq` x) $ File { name = n, hash = h }
  -- return File { name = n, hash = h }
  return $ (if depth < lazyDirDepth
              then id
              else (\x -> hash x `seq` name x `seq` x))
         $ File { name = n, hash = h, fileData = fd }

buildTree' readFileFn v depth es d@(a DT.:/ (DT.Dir n _)) = do
  let root = a </> n2fp n
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

  -- use lazy evaluation up to 5 levels deep, then strict
  return $ (if depth < lazyDirDepth
              then id
              else (\r -> (hash r `seq` nFiles r `seq` hash r) `seq` r))
         $ Dir
            { name     = n
            , contents = cs''
            , hash     = hashContents cs''
            , nFiles   = sum $ map countFiles cs''
            }


