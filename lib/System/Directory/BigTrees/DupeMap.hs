{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- Other than for printing and writing output to files, this module shouldn't
 - need any IO. That also means it shouldn't deal with encoding or decoding
 - `OsPath`s. Hopefully that can be kept in the app.
 -}

module System.Directory.BigTrees.DupeMap
  ( DupeSet
  , DupeTable
  , SortedDupeLists
  , SortedDupeSets
  , addTreeToDupeMap
  , dupesByNNodes
  , explainDupes
  , insertDupeSet
  , mergeDupeSets
  , pathsByHash
  , printDupes
  , scoreSets
  , simplifyDupes
  , writeDupes
  , hWriteDupes
  )
  where

import Control.Monad.ST (ST, runST)
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashSet as S
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Cuckoo as C
import Data.List (isPrefixOf, sort)
import qualified Data.List as L
import qualified Data.Massiv.Array as A
import System.Directory.BigTrees.Hash (Hash)
import System.Directory.BigTrees.Name (Name (..), n2op)
import System.Directory.BigTrees.HashLine (Depth (..), NNodes (..), TreeType (..))
import System.Directory.BigTrees.HashTree (HashTree (..), NodeData (..),
                                           ProdTree, treeType, treeHash, treeModTime, sumNodes, treeNBytes,
                                           treeName, SearchConfig (..))
import System.IO (Handle, IOMode (..))
import Data.Functor ((<&>))
import qualified System.File.OsPath as SFO
import System.OsPath (OsPath, (</>), splitDirectories, decodeFS)
import System.Directory.BigTrees.HashSet (HashSet, readHashList, hashSetFromList, emptyHashSet, setContainsHash)

-- TODO be able to serialize dupe tables for debugging
-- TODO can Foldable or Traversable simplify these?

-- TODO is DupeSet a Monoid?
-- TODO store paths as NamesFwd/NamesRev instead of OsPath?
-- TODO newtypes here? or strict data?
type DupeSet  = (Int, TreeType, S.HashSet OsPath)
type DupeList = (Int, TreeType, [OsPath])

-- TODO remove DupeMap type?
-- type DupeMap     = M.HashMap Hash DupeSet
type DupeTable s = C.HashTable s Hash DupeSet

-- TODO newtypes?
type SortedDupeSets  = [DupeSet]
type SortedDupeLists = [DupeList]


------------------------- DupeTable from HashTree -----------------------------

-- TODO what about if we guess the approximate size first?
-- TODO what about if we make it from the serialized hashes instead of a tree?
pathsByHash :: SearchConfig -> HashTree a -> ST s (DupeTable s)
pathsByHash cfg tree = do
  ht <- H.newSized 1 -- TODO size from top node of tree or from reference hashset
  addTreeToDupeMap cfg ht tree
  -- TODO try putting it back and compare overall speed
  -- H.mapM_ (\(k,_) -> H.mutate ht k removeNonDupes) ht
  return ht

-- inserts all nodes from a tree into an existing dupemap
-- TODO The empty string (mempty) behaves right, right? (disappears)
addTreeToDupeMap :: SearchConfig -> DupeTable s -> HashTree a -> ST s ()
addTreeToDupeMap cfg ht = addTreeToDupeMap' cfg ht mempty

-- same, but start from a given root path
-- TODO NamesFwd or NamesRev instead of OsPath?
addTreeToDupeMap' :: SearchConfig -> DupeTable s -> OsPath -> HashTree a -> ST s ()

addTreeToDupeMap' _ ht dir (Err {}) = return () -- TODO anything better to do with Errs?

-- Links can be "good" or "broken" based on whether their content should be in
-- the tree. But for dupes purposes, I'm not sure it matters. The hash will be
-- of the actual target or of the link itself, and either way it will go into a
-- corresponding dupeset.
addTreeToDupeMap' cfg ht dir l@(Link {})
  = insertDupeSet ht (treeHash l) (1, treeType l, S.singleton $ dir </> n2op (treeName l))

addTreeToDupeMap' cfg ht dir (File {nodeData=(NodeData{name=Name n, hash=h})})
  = insertDupeSet ht h (1, F, S.singleton $ dir </> n)

addTreeToDupeMap' cfg ht dir (Dir {nodeData=(NodeData{name=Name n, hash=h}), dirContents=cs, nNodes=(NNodes fs)}) = do
  insertDupeSet ht h (fs, D, S.singleton $ dir </> n)
  mapM_ (addTreeToDupeMap' cfg ht (dir </> n)) cs

-- inserts one node into an existing dupemap
insertDupeSet :: DupeTable s -> Hash -> DupeSet -> ST s ()
insertDupeSet ht h d2 = do
  existing <- H.lookup ht h
  case existing of
    Nothing -> H.insert ht h d2
    Just d1 -> H.insert ht h $ mergeDupeSets d1 d2

mergeDupeSets :: DupeSet -> DupeSet -> DupeSet
mergeDupeSets (n1, t, l1) (n2, _, l2) = (n1 + n2, t, S.union l1 l2)

-- TODO is this reasonable?
type DupeSetVec = A.Array A.BN A.Ix1 DupeSet

dupesByNNodes :: (forall s. ST s (DupeTable s)) -> SortedDupeLists
dupesByNNodes ht = simplifyDupes $ Prelude.map fixElem sortedL
  where
    sets     = runST $ scoreSets =<< ht
    unsorted = A.fromList A.Par sets :: DupeSetVec
    sorted   = A.quicksort $ A.compute unsorted :: DupeSetVec
    sortedL  = A.toList sorted
    fixElem (n, t, fs) = (negate n, t, L.sort $ S.toList fs)

{- This does a few things:
 - * removes singleton sets (no duplicates)
 - * adjusts the int scores from "n files in set" to "n files saved by dedup"
 - * negates scores so quicksort will put them in descending order
 -}
scoreSets :: C.HashTable s Hash DupeSet -> ST s SortedDupeSets 
scoreSets = H.foldM (\vs (_, v@(_,t,fs)) ->
  return $ if length fs > 1 then (negate $ score v,t,fs):vs else vs) []
  where
    -- TODO reference set case
    score (n, D, fs) = n - n `div` length fs
    score (n, _, _ ) = n - 1 -- TODO is this right?

{- Assumes a pre-sorted list as provided by dupesByNNodes.
 - Removes lists whose elements are all inside elements of the first list.
 - For example if the first is dir1, dir2, dir3
 - and the next is dir1/file.txt, dir2/file.txt, dir3/file.txt
 - ... then the second set is redundant and confusing to show.
 -}
simplifyDupes :: SortedDupeLists -> SortedDupeLists
simplifyDupes [] = []
simplifyDupes (d@(_,_,fs):ds) = d : filter (not . redundantSet) ds
  where
    redundantSet (_,_,fs') = all redundant fs'
    redundant e' = or [splitDirectories e
                       `isPrefixOf`
                       splitDirectories e' | e <- fs]

-- TODO subtract one group when saying how many dupes in a dir,
--      and 1 when saying how many in a file. because it's about how much you would save
printDupes :: SearchConfig -> SortedDupeLists -> IO ()
printDupes cfg groups = do
  msg <- explainDupes (maxDepth cfg) groups
  B8.putStrLn msg

writeDupes :: SearchConfig -> OsPath -> SortedDupeLists -> IO ()
writeDupes cfg path groups = do
  msg <- explainDupes (maxDepth cfg) groups
  SFO.writeFile' path msg

hWriteDupes :: SearchConfig -> Handle -> SortedDupeLists -> IO ()
hWriteDupes cfg hdl groups = do
  msg <- explainDupes (maxDepth cfg) groups
  B8.hPutStrLn hdl msg

explainDupes :: Maybe Depth -> SortedDupeLists -> IO B8.ByteString
explainDupes md ls = mapM explainGroup ls <&> B8.unlines
  where
    disclaimer Nothing  = ""
    disclaimer (Just (Depth d)) =
      " (up to " `B8.append` B8.pack (show d) `B8.append` " levels deep)"

    explainGroup :: DupeList -> IO B8.ByteString
    explainGroup (n, t, paths) = do
      paths' <- mapM decodeFS paths -- TODO is decoding necessary, even to write a script?
      return $ B8.unlines
             $ (header t n (length paths) `B8.append` ":")
             : sort (map B8.pack paths')

    header :: TreeType -> Int -> Int -> B8.ByteString
    header E _ _ = "" -- TODO is that a good idea?
    header D n ds = B8.intercalate " "
      [ "# deduping these" , B8.pack (show ds)
      , "dirs would remove", B8.pack (show n)
      , B8.append "files" (disclaimer md)
      ]
    header F n fs = B8.intercalate " "
      [ "# deduping these"  , B8.pack   (show fs)
      , "files would remove", B8.append (B8.pack $ show n) (disclaimer md)
      ]
    header _ n ls = B8.intercalate " "
      [ "# deduping these"  , B8.pack   (show ls)
      , "links would remove", B8.append (B8.pack $ show n) (disclaimer md)
      ]


------------------- filter which nodes are added to dupemaps ------------------

-- TODO is there a smarter way to combine this with findKeepNode in HashTree.Find?
--      it's almost the same except it includes rather than excludes the set
dupesKeepNode :: SearchConfig -> HashSet s -> Depth -> HashTree a -> ST s Bool
dupesKeepNode _ _ _ (Err {}) = return False -- TODO is this how we should handle them?
dupesKeepNode cfg eSet d t = do
  includeHash <- setContainsHash eSet $ treeHash t
  return $ and
    [ maybe True (d >=) $ minDepth cfg
    , maybe True (d <=) $ maxDepth cfg
    , maybe True (treeNBytes  t >=) $ minBytes cfg
    , maybe True (treeNBytes  t <=) $ maxBytes cfg
    , maybe True (sumNodes    t >=) $ minFiles cfg
    , maybe True (sumNodes    t <=) $ maxFiles cfg
    , maybe True (treeModTime t >=) $ minModtime cfg
    , maybe True (treeModTime t <=) $ maxModtime cfg
    , maybe True (treeType t `elem`) $ treeTypes cfg
    , includeHash
    ]


---------------------------------- tests --------------------------------------

-- TODO property: if you dedup a list of the same dir 2+ times,
-- there should only be one big overall dupe
-- TODO property: adding to the dupe set should be idempotent
