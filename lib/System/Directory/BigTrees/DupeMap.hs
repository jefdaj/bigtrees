{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- Other than for printing and writing output to files, this module shouldn't
 - need any IO. That also means it shouldn't deal with encoding or decoding
 - `OsPath`s. Hopefully that can be kept in the app.
 -}

module System.Directory.BigTrees.DupeMap
  ( DupeMap
  , DupeSet
  , SortedDupeLists
  , SortedDupeSets
  , addTreeToDupeMap
  , dupesByNegScore
  , explainDupes
  , hWriteDupes
  , insertDupeSet
  , mergeDupeSets
  , pathsByHash
  , scoreSetRef
  , scoreSetSelf
  , scoreSets
  , simplifyDupes
  )
  where

import Control.Monad.ST (ST)
import Control.Monad (when)
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

-- TODO be able to serialize dupemaps for debugging
-- TODO can Foldable or Traversable simplify these?

-- TODO is DupeSet a Monoid?
-- TODO store paths as NamesFwd/NamesRev instead of OsPath?
-- TODO newtypes here? or strict data?
type DupeSet  = (Int, TreeType, S.HashSet OsPath)
type DupeList = (Int, TreeType, [OsPath])

-- TODO remove DupeMap type?
-- type DupeMap     = M.HashMap Hash DupeSet
type DupeMap s = C.HashTable s Hash DupeSet

-- TODO newtypes?
type SortedDupeSets  = [DupeSet]
type SortedDupeLists = [DupeList]


------------------------------- create dupemaps -------------------------------

-- TODO what about if we guess the approximate size first?
-- TODO what about if we make it from the serialized hashes instead of a tree?
pathsByHash :: SearchConfig -> Maybe (HashSet s) -> HashTree a -> ST s (DupeMap s)
pathsByHash cfg mrSet tree = do
  ht <- H.newSized 1 -- TODO size from top node of tree or from reference hashset
  addTreeToDupeMap cfg mrSet ht tree
  -- TODO try putting it back and compare overall speed
  -- H.mapM_ (\(k,_) -> H.mutate ht k removeNonDupes) ht
  return ht

-- inserts all nodes from a tree into an existing dupemap
-- TODO The empty string (mempty) behaves rigdt, rigdt? (disappears)
addTreeToDupeMap :: SearchConfig -> Maybe (HashSet s) -> DupeMap s -> HashTree a -> ST s ()
addTreeToDupeMap cfg mrSet dt = addTreeToDupeMap' cfg mrSet dt mempty (Depth 0)

-- same, but start from a given root path
-- TODO NamesFwd or NamesRev instead of OsPath?
addTreeToDupeMap'
  :: SearchConfig
  -> Maybe (HashSet s)
  -> DupeMap s
  -> OsPath
  -> Depth
  -> HashTree a
  -> ST s ()

addTreeToDupeMap' _ _ dt dir _ (Err {}) = return () -- TODO anything better to do with Errs?

-- Links can be "good" or "broken" based on whether their content should be in
-- the tree. But for dupes purposes, I'm not sure it matters. The hash will be
-- of the actual target or of the link itself, and either way it will go into a
-- corresponding dupeset.
addTreeToDupeMap' cfg mrSet dt dir _ l@(Link {}) = do
  keepNode <- dupesKeepNode cfg mrSet l
  when keepNode $
    insertDupeSet cfg dt (treeHash l) (1, treeType l, S.singleton $ dir </> n2op (treeName l))

addTreeToDupeMap'
  cfg mrSet dt dir _
  f@(File {nodeData=(NodeData{name=Name n, hash=h})}) = do
    keepNode <- dupesKeepNode cfg mrSet f
    when keepNode $
      insertDupeSet cfg dt h (1, F, S.singleton $ dir </> n)

addTreeToDupeMap'
  cfg mrSet dt dir depth
  d@(Dir {nodeData=(NodeData{name=Name n, hash=h}), dirContents=cs, nNodes=(NNodes fs)}) = do
    keepNode <- dupesKeepNode cfg mrSet d
    let recurse = dupesRecurseChildren cfg depth d
    when keepNode $ insertDupeSet cfg dt h (fs, D, S.singleton $ dir </> n)
    when recurse  $ mapM_ (addTreeToDupeMap' cfg mrSet dt (dir </> n) (depth+1)) cs

-- inserts one node into an existing dupemap
-- TODO any reason not to pass the tree here instead? then all the "keepNode" stuff can go here
insertDupeSet :: SearchConfig -> DupeMap s -> Hash -> DupeSet -> ST s ()
insertDupeSet cfg ht h d2 = do
  existing <- H.lookup ht h
  case existing of
    Nothing -> H.insert ht h d2
    Just d1 -> H.insert ht h $ mergeDupeSets d1 d2

mergeDupeSets :: DupeSet -> DupeSet -> DupeSet
mergeDupeSets (n1, t, l1) (n2, _, l2) = (n1 + n2, t, S.union l1 l2)


-------------------------- quicksort dupetables by score ----------------------

-- TODO is this reasonable?
type DupeSetVec = A.Array A.BN A.Ix1 DupeSet

-- The negate here undoes the one in scoreSets below, leaving a positive score.
-- TODO is that the cleanest way to do it, or should both negates be in this fn?
dupesByNegScore :: ScoreFn -> DupeMap s -> ST s SortedDupeLists
dupesByNegScore scoreFn ht = do
  sets <- scoreSets scoreFn ht -- TODO separate scoring for ref set than within same tree
  let unsorted = A.fromList A.Par sets :: DupeSetVec
      sorted   = A.quicksort $ A.compute unsorted :: DupeSetVec
      sortedL  = A.toList sorted
      fixElem (n, t, fs) = (negate n, t, L.sort $ S.toList fs)
  return $ simplifyDupes $ Prelude.map fixElem sortedL

{- Assumes a pre-sorted list of lists.
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


-------------------------- score sets for quicksorting ------------------------

{- This does a few things:
 - * removes singleton sets (no duplicates)
 - * adjusts the int scores from "n files in set" to "n files saved by dedup"
 - * negates scores so quicksort will put them in descending order
 - TODO should length-1 sets not be rejected?
 -}
scoreSets :: ScoreFn -> C.HashTable s Hash DupeSet -> ST s SortedDupeSets 
scoreSets scoreFn = H.foldM (\vs (_, v@(_,t,fs)) ->
  return $ if length fs > 1 then (negate $ scoreFn v,t,fs):vs else vs) []

type ScoreFn = DupeSet -> Int

-- | This version is for dupes vs a reference set. It's simpler because there's
-- no need to leave out one canonical version from each dupe set.
scoreSetRef :: ScoreFn
scoreSetRef (n, _, _) = n -- TODO is that all? lol

-- | This version is for dupes within the tree itself, which is a little more
-- complicated because we want to save (not delete) one copy from each dupe
-- group.
scoreSetSelf :: ScoreFn
scoreSetSelf (n, D, fs) = n - n `div` length fs
scoreSetSelf (n, _, _ ) = n - 1 -- TODO is this right?


-------------------------------- write output ---------------------------------

hWriteDupes :: SearchConfig -> Handle -> SortedDupeLists -> IO ()
hWriteDupes cfg hdl groups = do
  msg <- explainDupes (maxDepth cfg) groups
  B8.hPutStr hdl msg

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
--      (oh, and no depth tests)
dupesKeepNode :: SearchConfig -> Maybe (HashSet s) -> HashTree a -> ST s Bool
dupesKeepNode _ _ (Err {}) = return False -- TODO is this how we should handle them?
dupesKeepNode cfg mrSet t = do
  includeHash <- case mrSet of
                   Nothing -> return True
                   Just rSet -> setContainsHash rSet $ treeHash t
  return $ and
    [ maybe True (treeNBytes  t >=) $ minBytes cfg
    , maybe True (treeNBytes  t <=) $ maxBytes cfg
    , maybe True (sumNodes    t >=) $ minFiles cfg
    , maybe True (sumNodes    t <=) $ maxFiles cfg
    , maybe True (treeModTime t >=) $ minModtime cfg
    , maybe True (treeModTime t <=) $ maxModtime cfg
    , maybe True (treeType t `elem`) $ treeTypes cfg
    , includeHash
    ]

-- | When adding a tree to a dupemap, whether to recurse into the tree's children.
-- The tree should always be a Dir, but we don't check for that here.
dupesRecurseChildren :: SearchConfig -> Depth -> HashTree a -> Bool
dupesRecurseChildren cfg d t = and
  [ maybe True (d <) $ maxDepth cfg
  , maybe True (treeNBytes  t > ) $ minBytes cfg
  , maybe True (sumNodes    t > ) $ minFiles cfg
  , maybe True (treeModTime t >=) $ minModtime cfg
  ]


---------------------------------- tests --------------------------------------

-- TODO property: if you dedup a list of the same dir 2+ times,
-- there should only be one big overall dupe
-- TODO property: adding to the dupe set should be idempotent
