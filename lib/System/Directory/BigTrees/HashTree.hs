{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.Directory.BigTrees.HashTree where
  -- ( HashTree(..)
  -- , ProdTree(..)
  -- , HashLine(..)
  -- , keepPath
  -- , readTree
  -- , buildTree
  -- , buildProdTree
  -- , readOrBuildTree
  -- , renameRoot
  -- , printTree
  -- , writeBinTree
  -- , serializeTree
  -- , writeTree
  -- , flattenTree
  -- , deserializeTree
  -- , hashContents
  -- , dropTo
  -- , treeContainsPath
  -- , treeContainsHash
  -- , addSubTree
  -- , rmSubTree
  -- , accTrees -- TODO hide this better?
  -- -- for testing
  -- , countFiles
  -- )
  -- where

-- TODO would be better to adapt AnchoredDirTree with a custom node type than re-implement stuff


import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as BS

import System.Directory.BigTrees.Util
import qualified System.Directory.Tree as DT

import Control.Monad (msum, when)
import qualified Control.Monad.Parallel as P
import Data.Either (fromRight)
import Data.Function (on)
import Data.List (delete, find, nubBy, partition, sort, sortBy)
import Data.Maybe (isJust)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist,
                         removePathForcibly)
import System.FilePath.Glob (MatchOptions (..), Pattern, matchWith)
import System.IO (IOMode (..), hClose, hFlush, stdout, withFile)
import System.IO.Unsafe (unsafeInterleaveIO)

import Prelude hiding (take)

import Control.Exception.Safe (catchAny)
import Data.Store (Store (..), decodeIO, encode)
import TH.Derive

import Control.DeepSeq
import GHC.Generics (Generic)

import qualified Data.Attoparsec.ByteString.Char8 as A8

import Data.Char (toLower)
import System.Directory.BigTrees.Hash
import System.Directory.BigTrees.HashLine
import System.FilePath
import System.Info (os)
import System.IO.Temp
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Monadic

{- A tree of file names matching (a subdirectory of) the annex,
 - where each dir and file node contains a hash of its contents.
 - TODO read and write files
 - TODO would also storing the number of files in each dir help, or timestamps?
 -}
-- data HashTree = DT.AnchoredDirTree Hash
--   deriving (Eq, Read, Show)
--   TODO rename name -> path?
data HashTree a
  = File
      { name     :: !FileName
      , hash     :: !Hash
      , fileData :: !a
      }
  | Dir
      { name     :: !FileName
      , hash     :: Hash
      , contents :: [HashTree a]
      , nFiles   :: Int
      }
  deriving (Generic, Ord, Read, Show)

-- We only need the file decoration for testing, so we can leave it off the production types
type ProdTree = HashTree ()

-- TODO disable this while testing to ensure deep equality?
instance Eq (HashTree a) where
  t1 == t2 = hash t1 == hash t2

-- TODO once there's also a dirData, should this be BiFunctor instead?
-- TODO should this also re-hash the file, or is that not part of the fileData idea?
instance Functor HashTree where
  fmap fn f@(File {}) = f { fileData = fn (fileData f) }
  fmap fn d@(Dir  {}) = d { contents = map (fmap fn) (contents d) }

-- TODO test functor identity law

instance NFData a => NFData (HashTree a)

-- https://hackage.haskell.org/package/store-0.7.2/docs/Data-Store-TH.html
$($(derive [d|
    instance Store a => Deriving (Store (HashTree a))
    |]))

excludeGlobs :: [Pattern]
             -> (DT.AnchoredDirTree a -> DT.AnchoredDirTree a)
excludeGlobs excludes (a DT.:/ tree) = a DT.:/ DT.filterDir (keep a) tree
  where
    keep a (DT.Dir  n _) = keepPath excludes (a </> n2p n)
    keep a (DT.File n _) = keepPath excludes (a </> n2p n)
    keep a b             = True

keepPath :: [Pattern] -> FilePath -> Bool
keepPath excludes path = not $ any (\ptn -> matchWith opts ptn path) excludes
  where
    opts = MatchOptions
             { matchDotsImplicitly = True
             , ignoreCase          = False
             , ignoreDotSlash      = True
             }

-- try to read as binary, and fall back to text if it fails
readTree :: Maybe Int -> FilePath -> IO ProdTree
readTree md path = catchAny
                    (B8.readFile path >>= decodeIO)
                    (\_ -> deserializeTree md <$> B8.readFile path)
--   (do
--      (hs :: [HashLine]) <- decodeIO =<< B8.readFile path
--      return $ snd $ head $ foldr accTrees [] hs)
--   (\_ -> fmap deserializeTree $ B8.readFile path)

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

-- TODO take this as a command-line argument
lazyDirDepth :: Int
lazyDirDepth = 4

-- TODO oh no, does AnchoredDirTree fail on cyclic symlinks?
buildTree' :: (FilePath -> IO a) -> Bool -> Int -> [Pattern] -> DT.AnchoredDirTree a -> IO (HashTree a)
-- TODO catch and re-throw errors with better description and/or handle them here
buildTree' _ _ _ _  (a DT.:/ (DT.Failed n e )) = error $ (a </> n2p n) ++ ": " ++ show e
buildTree' readFileFn v depth es (a DT.:/ (DT.File n _)) = do
  -- TODO how to exclude these?
  let fPath = a </> n2p n
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
  let root = a </> n2p n
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

hashContents :: [HashTree a] -> Hash
hashContents = hashBytes . B8.unlines . sort . map (BS.fromShort . unHash . hash)

-- If passed a file this assumes it contains hashes and builds a tree of them;
-- If passed a dir it will scan it first and then build the tree.
-- TODO don't assume??
readOrBuildTree :: Bool -> Maybe Int -> [Pattern] -> FilePath -> IO ProdTree
readOrBuildTree verbose mmaxdepth excludes path = do
  isDir  <- doesDirectoryExist path
  isFile <- doesFileExist      path
  if      isFile then readTree mmaxdepth path
  else if isDir then buildProdTree verbose excludes path
  else error $ "No such file: '" ++ path ++ "'"

-- for comparing two trees without getting hung up on different overall names
renameRoot :: FilePath -> ProdTree -> ProdTree
renameRoot newName tree = tree { name = p2n newName }

-------------------------------------
-- serialize and deserialize trees --
-------------------------------------

-- TODO can Foldable or Traversable simplify these?
-- TODO need to handle unicode here?
-- TODO does map evaluation influence memory usage?
-- TODO create a single ByteString rather than a list for compression?
serializeTree :: ProdTree -> [B8.ByteString]
serializeTree = map prettyHashLine . flattenTree

-- TODO remove and make this a special case of WriteTree? or vice versa?
printTree :: ProdTree -> IO ()
printTree = mapM_ printLine . flattenTree
  where
    -- TODO don't flush every line
    printLine l = putStrLn (B8.unpack $ prettyHashLine l) >> hFlush stdout

-- this uses a handle for streaming output, which turns out to be important for memory usage
-- TODO rename writeHashes? this is a confusing way to say that
writeTree :: FilePath -> ProdTree -> IO ()
writeTree path tree = withFile path WriteMode $ \h ->
  mapM_ (B8.hPutStrLn h) (serializeTree tree)

writeBinTree :: FilePath -> ProdTree -> IO ()
writeBinTree path tree = B8.writeFile path $ encode tree

flattenTree :: ProdTree -> [HashLine]
flattenTree = flattenTree' ""

-- TODO need to handle unicode here?
-- TODO does this affect memory usage?
flattenTree' :: FilePath -> ProdTree -> [HashLine]
flattenTree' dir (File n h ()  ) = [HashLine (F, IndentLevel $ length (splitPath dir), h, n)]
flattenTree' dir (Dir  n h cs _) = subtrees ++ [wholeDir]
  where
    subtrees = concatMap (flattenTree' $ dir </> n2p n) cs
    wholeDir = HashLine (D, IndentLevel $ length (splitPath dir), h, n)

-- TODO error on null string/lines?
-- TODO wtf why is reverse needed? remove that to save RAM
-- TODO refactor so there's a proper buildTree function and this uses it
-- TODO what about files with newlines in them? might need to split at \n(file|dir)
deserializeTree :: Maybe Int -> B8.ByteString -> ProdTree
deserializeTree md = snd . head . foldr accTrees [] . reverse . parseHashes md

countFiles :: HashTree a -> Int
countFiles (File {}  )    = 1
countFiles (Dir  _ _ _ n) = n

{- This one is confusing! It accumulates a list of trees and their indent
 - levels, and when it comes across a dir it uses the indents to determine
 - which files are children to put inside it vs which are siblings.
 -
 - If a value for d (max depth) is given, any line with an indent above that
 - will be dropped from the list to decrease memory usage.
 -}
-- accTrees :: Maybe Int -> HashLine -> [(Int, HashTree)] -> [(Int, HashTree)]
-- accTrees Nothing hl cs = accTrees' hl cs
-- accTrees (Just d) hl@(_, indent, _, _) cs
--   | indent > d = cs
--   | otherwise  = accTrees' hl cs

accTrees :: HashLine -> [(IndentLevel, ProdTree)] -> [(IndentLevel, ProdTree)]
accTrees (HashLine (t, IndentLevel i, h, p)) cs = case t of
  F -> cs ++ [(IndentLevel i, File p h ())]
  D -> let (children, siblings) = partition (\(IndentLevel i2, _) -> i2 > i) cs
           dir = Dir p h (map snd children)
                         (sum $ map (countFiles . snd) children)
       in siblings ++ [(IndentLevel i, dir)]

-------------------
-- search a tree --
-------------------

-- treeContainsPath :: HashTree -> FilePath -> Bool
-- treeContainsPath (File f1 _     ) f2 = f1 == f2
-- treeContainsPath (Dir  f1 _ cs _) f2
--   | f1 == f2 = True
--   | length (pathComponents f2) < 2 = False
--   | otherwise = let n   = head $ pathComponents f2
--                     f2' = joinPath $ tail $ pathComponents f2
--                 in if f1 /= n
--                   then False
--                   else any (\c -> treeContainsPath c f2') cs

treeContainsPath :: ProdTree -> FilePath -> Bool
treeContainsPath tree path = isJust $ dropTo tree path

dropTo :: ProdTree -> FilePath -> Maybe ProdTree
dropTo t@(File f1 _ ()  ) f2 = if n2p f1 == f2 then Just t else Nothing
dropTo t@(Dir  f1 _ cs _) f2
  | n2p f1 == f2 = Just t
  | length (pathComponents f2) < 2 = Nothing
  | otherwise = let n   = p2n $ head $ pathComponents f2
                    f2' = joinPath $ tail $ pathComponents f2
                in if f1 /= n
                  then Nothing
                  else msum $ map (`dropTo` f2') cs

treeContainsHash :: ProdTree -> Hash -> Bool
treeContainsHash (File _ h1 ()  ) h2 = h1 == h2
treeContainsHash (Dir  _ h1 cs _) h2
  | h1 == h2 = True
  | otherwise = any (`treeContainsHash` h2) cs

-- TODO if tree contains path, be able to extract it! need for rm

-------------------
-- add a subtree --
-------------------

-- TODO use this to implement hashing multiple trees at once?
wrapInEmptyDir :: FilePath -> ProdTree -> ProdTree
wrapInEmptyDir n t = Dir { name = p2n n, hash = h, contents = cs, nFiles = nFiles t }
  where
    cs = [t]
    h = hashContents cs

wrapInEmptyDirs :: FilePath -> ProdTree -> ProdTree
wrapInEmptyDirs p t = case pathComponents p of
  []     -> error "wrapInEmptyDirs needs at least one dir"
  [n]    -> wrapInEmptyDir n t
  (n:ns) -> wrapInEmptyDir n $ wrapInEmptyDirs (joinPath ns) t

-- TODO does the anchor here matter? maybe it's set to the full path accidentally
addSubTree :: ProdTree -> ProdTree -> FilePath -> ProdTree
addSubTree (File _ _ ()) _ _ = error "attempt to insert tree into a file"
addSubTree _ _ path | null (pathComponents path) = error "can't insert tree at null path"
addSubTree main sub path = main { hash = h', contents = cs', nFiles = n' }
  where
    comps  = pathComponents path
    p1     = head comps
    path'  = joinPath $ tail comps
    h'     = hashContents cs'
    cs'    = sortBy (compare `on` name) $ filter (\c -> name c /= p2n p1) (contents main) ++ [newSub]
    n'     = nFiles main + nFiles newSub - maybe 0 nFiles oldSub
    sub'   = sub { name = p2n $ last comps }
    oldSub = find (\c -> name c == p2n p1) (contents main)
    newSub = if length comps == 1
               then sub'
               else case oldSub of
                 Nothing -> wrapInEmptyDirs path sub'
                 Just d  -> addSubTree d sub' path'

----------------------
-- remove a subtree --
----------------------

{- This one gets a little complicated because if the subtree exists
 - then after removing it we have to adjust parent nFiles back up to the root.
 - Also edits have to be done on the parent tree (so no File branch).
 - Buuuut for now can just ignore nFiles as it's not needed for the rm itself.
 - TODO does this actually solve nFiles too?
 -}
rmSubTree :: ProdTree -> FilePath -> Either String ProdTree
rmSubTree (File _ _ ()) p = Left $ "no such subtree: '" ++ p ++ "'"
rmSubTree d@(Dir _ _ cs n) p = case dropTo d p of
  Nothing -> Left $ "no such subtree: '" ++ p ++ "'"
  Just t -> Right $ if t `elem` cs
    then d { contents = delete t cs, nFiles = n - countFiles t }
    else d { contents = map (\c -> fromRight c $ rmSubTree c $ joinPath $ tail $ splitPath p) cs
           , nFiles = n - countFiles t
           }

-- A HashTree where file contents are generated by QuickCheck and stored in
-- memory for round-trip tests
type TestTree = HashTree B8.ByteString

parseHashLine :: B8.ByteString -> Either String (Maybe HashLine)
parseHashLine bs = A8.parseOnly (lineP Nothing) (B8.append bs "\n")

-----------
-- tests --
-----------

instance Arbitrary TreeType where

  arbitrary = do
    n <- choose (0,1 :: Int)
    return $ [F, D] !! n

  -- you could shrink D -> F, but not without changing the rest of the hashline
  shrink _ = []

instance Arbitrary IndentLevel where
  arbitrary = IndentLevel <$> ((arbitrary :: Gen Int) `suchThat` (>= 0))
  shrink _ = []

instance Arbitrary Hash where
  arbitrary = fmap hashBytes (arbitrary :: Gen B8.ByteString)
  shrink _ = []

-- TODO can you really have an arbitrary hashline without the rest of a tree?
instance Arbitrary HashLine where

  arbitrary = do
    tt <- arbitrary :: Gen TreeType
    il <- arbitrary :: Gen IndentLevel
    h  <- arbitrary :: Gen Hash
    n  <- arbitrary :: Gen FileName
    return $ HashLine (tt, il, h, n)

  -- only shrinks the filename
  shrink (HashLine (tt, il, h, n)) = map (\n' -> HashLine (tt, il, h, n')) (shrink n)

-- for removing duplicate filenames using nubBy, taking into account
-- case-insensitivity on apple filesystem
duplicateFilenames :: HashTree a -> HashTree a -> Bool
duplicateFilenames = if os == "darwin" then macDupes else unixDupes
  where
    macDupes  a b = map toLower (n2p $ name a)
                 == map toLower (n2p $ name b)
    unixDupes a b = n2p (name a)
                 == n2p (name b)

-- This is specialized to (HashTree B8.ByteString) because it needs to use the
-- same arbitrary bytestring for the file content and its hash
instance Arbitrary TestTree where

  arbitrary = do
    n <- arbitrary :: Gen FileName
    -- TODO there's got to be a better way, right?
    i <- choose (0,5 :: Int)
    if i == 0

      then do
        !cs <- nubBy duplicateFilenames <$> resize 5 (arbitrary :: Gen [TestTree])
        return $ Dir { name     = n
                     , hash     = hashContents cs
                     , contents = cs
                     , nFiles   = sum $ map countFiles cs
                     }

      else do
        bs <- arbitrary :: Gen B8.ByteString
        return $ File { name = n
                      , hash = hashBytes bs
                      , fileData = bs
                      }

  -- only shrinks the filename
  shrink f@(File {}) = map (\n -> f { name = n }) (shrink $ name f)

  -- shrinks either the name or the contents, and adjusts the rest to match
  shrink d@(Dir {}) = newNames ++ newContents
    where
      newNames = map (\n -> d { name = n }) (shrink $ name d)
      newContents = map (\cs -> d { contents = cs
                                  , hash = hashContents cs
                                  , nFiles = sum $ map countFiles cs})
                        (shrink $ contents d)

instance Arbitrary ProdTree where
  arbitrary = fmap dropFileData arbitrary

-- TODO rename the actual function file -> fileData to match future dirData
-- TODO rewrite this in terms of a generic map/fold so it works with other types
dropFileData :: TestTree -> ProdTree
dropFileData d@(Dir {contents = cs}) = d {contents = map dropFileData cs}
dropFileData f@(File {})             = f {fileData = ()}

-- TODO test tree in haskell
-- TODO test dir
-- TODO test annex

-- TODO unit_build_tree_from_dir
-- TODO read_tree
-- TODO serialize_tree
-- TODO write_tree
-- TODO print_tree
-- TODO write_tree_binary?
-- TODO flatten_tree

-- prop_roundtrip_prodtree_to_hashes ::

--     describe "HashTree" $ do
--       describe "HashTree" $ do
--         it "builds a tree from the test annex" $ pendingWith "need annex test harness"

confirmFileHashes :: TestTree -> Bool
confirmFileHashes (File {fileData = f, hash = h}) = hashBytes f == h
confirmFileHashes (Dir {contents = cs})           = all confirmFileHashes cs

prop_confirm_file_hashes :: TestTree -> Bool
prop_confirm_file_hashes = confirmFileHashes

-- TODO prop_confirm_dir_hashes too?

-- TODO what's right here but wrong in the roundtrip to bytestring ones?
prop_roundtrip_prodtree_to_bytestring :: ProdTree -> Bool
prop_roundtrip_prodtree_to_bytestring t = t' == t
  where
    bs = B8.unlines $ serializeTree t -- TODO why didn't it include the unlines part again?
    t' = deserializeTree Nothing bs

-- TODO round-trip to binary files too

roundtrip_prodtree_to_hashes :: ProdTree -> IO ProdTree
roundtrip_prodtree_to_hashes t = withSystemTempFile "roundtriptemp" $ \path hdl -> do
  hClose hdl
  writeTree path t
  readTree Nothing path

prop_roundtrip_prodtree_to_hashes :: Property
prop_roundtrip_prodtree_to_hashes = monadicIO $ do
  t1 <- pick arbitrary
  t2 <- run $ roundtrip_prodtree_to_hashes t1
  assert $ t2 == t1

-- TODO separate thing for test and production trees here?
roundtrip_prodtree_to_bin_hashes :: ProdTree -> IO ProdTree
roundtrip_prodtree_to_bin_hashes t = withSystemTempFile "roundtriptemp" $ \path hdl -> do
  hClose hdl
  writeBinTree path t
  readTree Nothing path

prop_roundtrip_prodtree_to_bin_hashes :: Property
prop_roundtrip_prodtree_to_bin_hashes = monadicIO $ do
  t1 <- pick (arbitrary :: Gen ProdTree)
  t2 <- run $ roundtrip_prodtree_to_bin_hashes t1
  assert $ t2 == t1

-- this is to catch the case where it tries to write the same file twice
-- (happened once because of macos filename case-insensitivity)
assertNoFile :: FilePath -> IO ()
assertNoFile path = do
  exists <- doesPathExist path
  when exists $ error $ "duplicate write to: '" ++ path ++ "'"

{- Take a generated `TestTree` and write it to a tree of tmpfiles.
 - Note that this calls itself recursively.
 -}
writeTestTreeDir :: FilePath -> TestTree -> IO ()
writeTestTreeDir root (File {name = n, fileData = bs}) = do
  let path = root </> n2p n
  assertNoFile path
  B8.writeFile path bs
writeTestTreeDir root (Dir {name = n, contents = cs}) = do
  let root' = root </> n2p n
  assertNoFile root'
  -- putStrLn $ "write test dir: " ++ root'
  createDirectoryIfMissing True root' -- TODO false here
  mapM_ (writeTestTreeDir root') cs

readTestTree :: Maybe Int -> Bool -> [Pattern] -> FilePath -> IO TestTree
readTestTree md = buildTree B8.readFile

-- the tests above round-trip to single files describing trees, whereas this
-- one round-trips to an actual directory tree on disk
-- note that you have to drop the bytestrings from the original testtree to compare them
roundtrip_testtree_to_dir :: TestTree -> IO TestTree
roundtrip_testtree_to_dir t = withSystemTempDirectory "roundtriptemp" $ \root -> do
  let tmpRoot = "/tmp/round-trip-tests" -- TODO replace with actual root
  createDirectoryIfMissing True tmpRoot -- TODO False?
  let treePath = tmpRoot </> n2p (name t)
  removePathForcibly treePath -- TODO remove
  writeTestTreeDir tmpRoot t
  -- putStrLn $ "treePath: " ++ treePath
  readTestTree Nothing False [] treePath

prop_roundtrip_testtree_to_dir :: Property
prop_roundtrip_testtree_to_dir = monadicIO $ do
  t1 <- pick arbitrary
  t2 <- run $ roundtrip_testtree_to_dir t1
  assert $ force t2 == t1 -- force evaluation to prevent any possible conflicts
