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
  , addToDupeMap
  , dupesByNNodes
  , explainDupes
  , insertDupeSet
  , mergeDupeSets
  , pathsByHash
  , printDupes
  , scoreSets
  , simplifyDupes
  , writeDupes
  -- , DupeMap
  -- , allDupes
  -- , anotherCopy
  -- , listAllFiles
  -- , listLostFiles
  )
  where

import Control.Monad.ST (ST, runST)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Cuckoo as C
import Data.List (isPrefixOf, sort)
import qualified Data.List as L
import qualified Data.Massiv.Array as A
import System.Directory.BigTrees.Hash (Hash)
import System.Directory.BigTrees.HashLine (Depth (..), NNodes (..), TreeType (..))
import System.Directory.BigTrees.HashTree (HashTree (..), NodeData (..), ProdTree)
import System.Directory.BigTrees.HashTree.Search (SearchConfig (..))
import System.Directory.BigTrees.Name (Name (..), n2bs)
-- import System.OsPath (splitDirectories, (</>))

import Data.Functor ((<&>))
import qualified System.File.OsPath as SFO
import System.OsPath

-- TODO be able to serialize dupe tables for debugging

-- TODO are the paths getting messed up somewhere in here?
-- like this: myfirstdedup/home/user/bigtrees/demo/myfirstdedup/unsorted/backup/backup

-- TODO can Foldable or Traversable simplify these?

-- TODO is DupeSet a Monoid?
-- TODO store paths as NamesRev instead of OsPath?
-- TODO newtypes here? or strict data?
type DupeSet  = (Int, TreeType, S.HashSet OsPath)
type DupeList = (Int, TreeType, [OsPath])

-- TODO remove DupeMap type?
-- type DupeMap     = M.HashMap Hash DupeSet
type DupeTable s = C.HashTable s Hash DupeSet

--------------------------------
-- DupeTable from hashes file --
--------------------------------

-- TODO is this a good idea? it would require losing the nfiles per dir thing

-- TODO is this the right type signature for an stToIO action?
-- dupesFromHashes :: OsPath -> DupeTable RealWorld
-- dupesFromHashes = undefined

-----------------------------
-- DupeTable from HashTree --
-----------------------------

-- TODO what about if we guess the approximate size first?
-- TODO what about if we make it from the serialized hashes instead of a tree?
pathsByHash :: HashTree () -> ST s (DupeTable s)
pathsByHash t = do
  ht <- H.newSized 1 -- TODO size from tree
  addToDupeMap ht t
  -- TODO try putting it back and compare overall speed
  -- H.mapM_ (\(k,_) -> H.mutate ht k removeNonDupes) ht
  return ht

-- inserts all nodes from a tree into an existing dupemap in ST s
-- TODO The empty string (mempty) behaves right, right? (disappears)
addToDupeMap :: DupeTable s -> ProdTree -> ST s ()
addToDupeMap ht = addToDupeMap' ht mempty

-- same, but start from a given root path
addToDupeMap' :: DupeTable s -> OsPath -> ProdTree -> ST s ()
addToDupeMap' ht dir (File {nodeData=(NodeData{name=Name n, hash=h})}) = insertDupeSet ht h (1, F, S.singleton $ dir </> n)
addToDupeMap' ht dir (Dir {nodeData=(NodeData{name=Name n, hash=h}), dirContents=cs, nNodes=(NNodes fs)}) = do
  insertDupeSet ht h (fs, D, S.singleton $ dir </> n)
  mapM_ (addToDupeMap' ht (dir </> n)) cs

-- inserts one node into an existing dupemap in ST s
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

dupesByNNodes :: (forall s. ST s (DupeTable s)) -> [DupeList]
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
scoreSets :: C.HashTable s Hash DupeSet -> ST s [DupeSet]
scoreSets = H.foldM (\vs (_, v@(_,t,fs)) ->
  return $ if length fs > 1 then (negate $ score v,t,fs):vs else vs) []
  where
    score (n, D, fs) = n - n `div` length fs
    score (n, F, _ ) = n - 1

-- TODO could this be faster than quicksorting everything even though single threaded?
-- usage: H.mapM_ (\(k,_) -> H.mutate dt k removeNonDupes) dt
-- rewrite of `filter hasDupes` for use with H.mutate
-- removeNonDupes :: Maybe DupeSet -> (Maybe DupeSet, ())
-- removeNonDupes Nothing = (Nothing, ())
-- removeNonDupes (Just v@(nfiles, _, paths)) = (if S.size paths > 1 && nfiles > 0
--                                                 then Just v
--                                                 else Nothing, ())

{- Assumes a pre-sorted list as provided by dupesByNNodes.
 - Removes lists whose elements are all inside elements of the first list.
 - For example if the first is dir1, dir2, dir3
 - and the next is dir1/file.txt, dir2/file.txt, dir3/file.txt
 - ... then the second set is redundant and confusing to show.
 -}
simplifyDupes :: [DupeList] -> [DupeList]
simplifyDupes [] = []
simplifyDupes (d@(_,_,fs):ds) = d : filter (not . redundantSet) ds
  where
    redundantSet (_,_,fs') = all redundant fs'
    redundant e' = or [splitDirectories e
                       `isPrefixOf`
                       splitDirectories e' | e <- fs]

-- sorts paths by shallowest (fewest dirs down), then length of filename,
-- then finally alphabetical
-- TODO is it inefficient enough to slow down the dupes command? rewrite if so
-- sortDupePaths :: (Hash, DupeSet) -> (Hash, DupeList)
-- sortDupePaths (h, (i, t, ps)) = (h, (i, t, sortBy myCompare $ S.toList ps))
--   where
--     myCompare p1 p2 = let d1 = length $ splitDirectories $ B.unpack p1
--                           d2 = length $ splitDirectories $ B.unpack p2
--                           l1 = length $ B.unpack p1
--                           l2 = length $ B.unpack p2
--                       in if      d1 > d2 then GT
--                          else if d1 < d2 then LT
--                          else if l1 > l2 then GT
--                          else if l1 < l2 then LT
--                          else compare p1 p2

-- hasDupes :: (Hash, DupeSet) -> Bool
-- hasDupes (_, (nfiles, _, paths)) = S.size paths > 1 && nfiles > 0

-- TODO use this as the basis for the dedup repl
-- TODO subtract one group when saying how many dupes in a dir,
--      and 1 when saying how many in a file. because it's about how much you would save
printDupes :: SearchConfig -> [DupeList] -> IO ()
printDupes cfg groups = do
  msg <- explainDupes (maxDepth cfg) groups
  B.putStrLn msg

writeDupes :: SearchConfig -> OsPath -> [DupeList] -> IO ()
writeDupes cfg path groups = do
  msg <- explainDupes (maxDepth cfg) groups
  SFO.writeFile' path msg

explainDupes :: Maybe Depth -> [DupeList] -> IO B.ByteString
explainDupes md ls = mapM explainGroup ls <&> B.unlines
  where
    disclaimer Nothing  = ""
    disclaimer (Just (Depth d)) =
      " (up to " `B.append` B.pack (show d) `B.append` " levels deep)"

    explainGroup :: DupeList -> IO B.ByteString
    explainGroup (n, t, paths) = do
      paths' <- mapM decodeFS paths
      return $ B.unlines
             $ (header t n (length paths) `B.append` ":")
             : sort (map B.pack paths')

    header :: TreeType -> Int -> Int -> B.ByteString
    header F n fs = B.intercalate " " [ "# deduping these"  , B.pack (show fs)
      , "files would remove", B.append (B.pack (show n )) (disclaimer md)
      ]
    header D n ds = B.intercalate " " [ "# deduping these" , B.pack (show ds)
      , "dirs would remove", B.pack (show n )
      , B.append "files" (disclaimer md)
      ]

-----------------------------
-- info about copy numbers --
-----------------------------

-- TODO remove entire section?

-- TODO remove? works, but not sure if ever needed
-- listAllFiles :: OsPath -> ProdTree -> [(Hash, OsPath)]
-- listAllFiles anchor (File {nodeData=(NodeData{name=Name n, hash=h})}) = [(h, anchor </> n)]
-- listAllFiles anchor (Dir {nodeData=(NodeData{name=Name n}), dirContents=cs}) =
--   concatMap (listAllFiles $ anchor </> n) cs


-- TODO rewrite allDupes by removing the subtree first then testing membership
--      (that way can use the removing part separately in cmdDedup)

-- helper for allDupes
-- TODO how to make the lookups safe?
-- TODO remove? works, but not sure if ever needed
-- anotherCopy :: Hash -> DupeMap -> DupeMap -> Bool
-- anotherCopy h mainMap subMap = nMain > nSub
--   where
--     (Just nMain) = (\(n,_,_) -> n) <$> M.lookup h mainMap
--     (Just nSub ) = (\(n,_,_) -> n) <$> M.lookup h subMap

-- TODO finish this
-- TODO remove? not sure if ever needed
--allDupes :: ProdTree -> ProdTree -> Bool
---- allDupes mainTree subTree = all safeToRmHash $ undefined subDupes
--allDupes mainTree subTree = undefined safeToRmHash $ undefined subDupes
--  where
--    mainDupes = undefined $ pathsByHash mainTree
--    subDupes  = undefined $ pathsByHash subTree
--    safeToRmHash h = anotherCopy h mainDupes subDupes

-- TODO remove? not sure if ever needed
-- for warning the user when their action will delete the last copy of a file
-- TODO also warn about directories, because sometimes they might care (Garageband files for example)
-- TODO make more efficient by restricting to hashes found in the removed subtree!
--      (only used for Rm right?)
--listLostFiles :: HashTree () -> HashTree () -> [OsPath]
--listLostFiles before after = filesLost
--  where
--    hashesBefore = pathsByHash before
--    hashesAfter  = pathsByHash after
--    hashesLost   = undefined hashesBefore hashesAfter
--    filesLost    = sort $ S.toList $ S.unions $ M.elems
--                 $ M.map (\(_,_,fs) -> fs)
--                 $ M.filter (\(_,t,_) -> t == F) hashesLost

-----------
-- tests --
-----------

-- TODO property: if you dedup a list of the same dir 2+ times, there should only be one big overall dupe
-- TODO property: adding to the dupe set should be idempotent

