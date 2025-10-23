{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- Other than for printing and writing output to files, this module shouldn't
 - need any IO. That also means it shouldn't deal with encoding or decoding
 - `OsPath`s. Hopefully that can be kept in the app.
 -}

module System.Directory.BigTrees.DupeMap
  ( DupeMap
  , DupeSet
  , DupeList
  , SortedDupeLists
  , SortedDupeSets
  , AddTreeProgress
  , addTreeToDupeMap
  , dupesByNegScore
  , insertDupeSet
  , mergeDupeSets
  , pathsByHash
  , scoreSetRef
  , scoreSetSelf
  , scoreSets
  , sortPaths
  , simplifyDupes
  )
  where

import Control.DeepSeq (deepseq)
import Control.Monad.ST (ST)
import Control.Monad (when)
import qualified Data.ByteString.Char8 as B8
import Data.Functor ((<&>))
import qualified Data.HashSet as S
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Cuckoo as C
import Data.Ord (comparing)
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Massiv.Array as A
import System.Directory.BigTrees.Hash (Hash, unHash, prettyHash)
import System.Directory.BigTrees.Name (Name (..), n2op, op2ns, breadcrumbs2bs, op2s)
import System.Directory.BigTrees.HashLine (Depth (..), NNodes (..), TreeType (..))
import System.Directory.BigTrees.HashTree (HashTree (..), NodeData (..),
                                           ProdTree, treeType, treeHash, treeModTime, treeNNodes, treeNBytes,
                                           treeName, SearchConfig (..))
import System.Directory.BigTrees.Logging (LogCfg (..), LogLevel (..), LogContext, logUnsafe, die, addLogContext)
import System.IO (Handle, IOMode (..))
import Data.Functor ((<&>))
import qualified System.File.OsPath as SFO
import System.OsPath (OsPath, (</>), joinPath, splitDirectories, decodeFS)
import System.Directory.BigTrees.HashSet (HashSet, readHashList, hashSetFromList, emptyHashSet, setContainsHash)

import System.Directory.BigTrees.HashTree.Search (LabeledSearches, Search (..), SearchConfig (..),
                                                  SearchLabel, CompiledSearch (..), CompiledLabeledSearches, treeContainsPath, compileLabeledSearches)

import System.Directory.BigTrees.HashTree.Find (findLabelNode)
import Data.Maybe (isNothing)
import Data.STRef (STRef(..), newSTRef, readSTRef, writeSTRef)
import qualified Data.ByteString.Short as SBS
import System.Directory.BigTrees.Util (sbs2b8)

-- TODO be able to serialize dupemaps for debugging
-- TODO can Foldable or Traversable simplify these?

-- TODO is DupeSet a Monoid?
-- TODO store paths as NamesFwd/NamesRev instead of OsPath?
-- TODO newtypes here? or strict data?
type DupeSet  = (Int, Hash, TreeType, S.HashSet OsPath) -- TODO remove hash here?
type DupeList = (Int, Hash, TreeType, [OsPath])

type DupeMap s = C.HashTable s Hash DupeSet

-- TODO newtypes?
type SortedDupeSets  = [DupeSet]
type SortedDupeLists = [DupeList]

-- For logging progress in addTreeToDupeMap
-- N nodes added so far out of N total
-- TODO more general STProgress or similar?
-- TODO another Int for N total? only if possible without forcing evaluation
-- TODO does it impact performance significantly?
newtype AddTreeProgress = AddTreeProgress Int
  deriving (Eq, Ord, Num, Read, Show)

-- Update tree adding progress and log it if appropriate
-- TODO and think/rethink about the ST type
-- incTreeProgress :: LogCfg -> AddTreeProgress -> ST s AddTreeProgress
-- incTreeProgress lCfg (AddTreeProgress nSoFar nTotal) = AddTreeProgress (nSoFar + 1) nTotal

------------------------------- create dupemaps -------------------------------

-- TODO unify with incLogProgressST in Logging
incAddTreeProgress :: LogCfg -> STRef s AddTreeProgress -> ST s ()
incAddTreeProgress lCfg progressRef = do
  n <- readSTRef progressRef
  let n'@(AddTreeProgress nNodes) = n + 1
      msg = "added " <> B8.pack (show nNodes) <> " nodes"
      action = writeSTRef progressRef n'
  -- log only every 1000 nodes
  -- TODO make this configurable or auto-adjust?
  if nNodes `mod` 1000 == 0
     then logUnsafe (addLogContext lCfg "addTreeToDupeMap") InfoL msg action
     else action

-- TODO what about if we guess the approximate size first?
-- TODO what about if we make it from the serialized hashes instead of a tree?
pathsByHash
  :: SearchConfig -> LogCfg -> Maybe (HashSet s) -> CompiledLabeledSearches
  -> HashTree a -> ST s (DupeMap s)
pathsByHash cfg lCfg mrSet cle tree = do
  -- let (NNodes n) = treeNNodes tree TODO does this force evaluation??
      -- info msg = logUnsafe lCfg InfoL "pathsByHash" msg $ return ()
  -- TODO is it more wasteful to allocate it too large like this, or to expand it?
  dm <- H.newSized 1000 -- n
  -- info $ "adding " <> B8.pack (show n) <> " nodes to hashmap" -- TODO inside addTreeToDupeMap?
  addTreeToDupeMap cfg lCfg mrSet cle dm tree
  -- TODO try putting it back and compare overall speed
  -- H.mapM_ (\(k,_) -> H.mutate dm k removeNonDupes) dm
  return dm

-- inserts all nodes from a tree into an existing dupemap
-- TODO The empty string (mempty) behaves right, right? (disappears)
addTreeToDupeMap
  :: SearchConfig -> LogCfg -> Maybe (HashSet s) -> CompiledLabeledSearches
  -> DupeMap s -> HashTree a -> ST s ()
addTreeToDupeMap cfg lCfg mrSet cle dm t = do
  pRef <- newSTRef $ AddTreeProgress 0
  addTreeToDupeMap' cfg lCfg mrSet cle dm mempty (Depth 0) pRef t

-- same, but start from a given root path
-- TODO NamesFwd or NamesRev instead of OsPath?
addTreeToDupeMap'
  :: SearchConfig
  -> LogCfg
  -> Maybe (HashSet s)
  -> CompiledLabeledSearches
  -> DupeMap s
  -> OsPath
  -> Depth
  -> STRef s AddTreeProgress
  -> HashTree a
  -> ST s ()

-- TODO log errors here?
addTreeToDupeMap' _ _ _ _ dm dir _ _ (Err {}) = return ()

-- Links can be "good" or "broken" based on whether their content should be in
-- the tree. But for dupes purposes, I'm not sure it matters. The hash will be
-- of the actual target or of the link itself, and either way it will go into a
-- corresponding dupeset.
addTreeToDupeMap' cfg lCfg mrSet cle dm dir d pr l@(Link {nodeData=NodeData {hash=h}}) = do
  keepNode <- dupesKeepNode cfg lCfg mrSet cle (op2ns dir) d l
  when keepNode $
    insertDupeSet cfg lCfg dm (treeHash l) (1, h, treeType l, S.singleton $ dir </> n2op (treeName l)) pr

addTreeToDupeMap'
  cfg lCfg mrSet cle dm dir d pr
  f@(File {nodeData=(NodeData{name=Name n, hash=h})}) = do
    keepNode <- dupesKeepNode cfg lCfg mrSet cle (op2ns dir) d f
    when keepNode $
      insertDupeSet cfg lCfg dm h (1, h, F, S.singleton $ dir </> n) pr

addTreeToDupeMap'
  cfg lCfg mrSet cle dm dir depth pr
  d@(Dir {nodeData=(NodeData{name=Name n, hash=h}), dirContents=cs, nNodes=(NNodes fs)}) = do
    keepNode <- dupesKeepNode cfg lCfg mrSet cle (op2ns dir) depth d
    let recurse = dupesRecurseChildren cfg depth d
    when keepNode $ do
      insertDupeSet cfg lCfg dm h (fs, h, D, S.singleton $ dir </> n) pr
    when recurse $
      mapM_ (addTreeToDupeMap' cfg lCfg mrSet cle dm (dir </> n) (depth+1) pr) cs

-- inserts one node into an existing dupemap
-- TODO any reason not to pass the tree here instead? then all the "keepNode" stuff can go here
insertDupeSet :: SearchConfig -> LogCfg -> DupeMap s -> Hash -> DupeSet -> STRef s AddTreeProgress -> ST s ()
insertDupeSet cfg lCfg dm h d2 pRef = do
  let debug  = logUnsafe (addLogContext lCfg "insertDupeSet") DebugL
      showH  = sbs2b8 $ unHash h
      showD2 = B8.pack $ show d2
  existing <- H.lookup dm h
  case existing of
    Nothing ->
      let msg = showH <> " init with " <> showD2
      in debug msg $ H.insert dm h d2
    Just d1@(_,_,_,ps) ->
      let n   = B8.pack $ show $ length ps
          msg = showH <> " size " <> n <> " add " <> showD2
      in debug msg $ H.insert dm h $ mergeDupeSets lCfg d1 d2
  incAddTreeProgress lCfg pRef

-- TODO is DupeSet a Monoid? or not, because there are some you can't merge?
mergeDupeSets :: LogCfg -> DupeSet -> DupeSet -> DupeSet
mergeDupeSets lCfg (n1, h1, t1, l1) d2@(n2, h2, t2, l2) = (n1 + n2, h, t, S.union l1 l2)
  where
    die' = die $ addLogContext lCfg "mergeDupeSets"
    h = if h1 == h2 then h1 else die' $ showH1 <> " /= " <> showH2
    t = if t1 == t2 then t1 else die' $ showH <> " " <> showT1 <> " /= " <> showT2 <> " " <> showD2
    showH1 = sbs2b8 $ unHash h1
    showH2 = sbs2b8 $ unHash h2
    showH  = sbs2b8 $ unHash h
    showT1 = B8.pack $ show t1
    showT2 = B8.pack $ show t2
    showD2 = B8.pack $ show d2


-------------------------- quicksort dupetables by score ----------------------

-- TODO is this reasonable?
type DupeSetVec = A.Array A.BN A.Ix1 DupeSet

-- The negate here undoes the one in scoreSets below, leaving a positive score.
-- TODO is that the cleanest way to do it, or should both negates be in this fn?
dupesByNegScore :: LogCfg -> ScoreFn -> DupeMap s -> ST s SortedDupeLists
dupesByNegScore lCfg scoreFn dm = do
  let debug = logUnsafe (addLogContext lCfg "dupesByNegScore") DebugL
  sets <- debug "scoring sets" <$> scoreSets scoreFn dm -- TODO separate scoring for ref set than within same tree
  let unsorted = debug "creating DupeSetVec" $ A.fromList A.Par $ deepseq sets sets :: DupeSetVec
      sorted   = debug "quicksorting DupeSetVec" $ A.quicksort $ A.compute $ deepseq unsorted unsorted :: DupeSetVec
      sortedL  = debug "converting DupeSetVec back to list" $ A.toList $ deepseq sorted sorted
      fixElem (n, h, t, fs) = (negate n, h, t, L.sort $ S.toList fs) -- TODO n before h?
      fixed    = Prelude.map fixElem $ deepseq sortedL sortedL
      simple = debug "simplifying dupes" $ simplifyDupes 1 lCfg $ deepseq fixed fixed -- TODO helps?
  return simple

{- Assumes a pre-sorted list of lists.
 - Removes lists whose elements are all inside elements of the first list.
 - For example if the first is dir1, dir2, dir3
 - and the next is dir1/file.txt, dir2/file.txt, dir3/file.txt
 - ... then the second set is redundant and confusing to show.
 -}
simplifyDupes :: Int -> LogCfg -> SortedDupeLists -> SortedDupeLists

simplifyDupes _ _ [ ] = [ ]
simplifyDupes _ _ [d] = [d]

simplifyDupes i lCfg (d@(_,h,D,fs):ds) = info msg $ (d:) $ simplifyDupes (i+1) lCfg $ ds'
  where
    showH = sbs2b8 $ unHash h
    showI = B8.pack $ show i
    showR = B8.pack $ show nRemain
    showD = B8.pack $ show nDrop
    msg = "iteration " <> showI <>
          " drop " <> showD <>
          " sets redundant with " <> showH <> "; " <> showR <>
          " sets remain to process"
    ds' = filter (not . redundantSet lCfg h fs) ds
    nRemain = length ds'
    nDrop = length ds - nRemain
    info msg x = if nDrop > 0
       then logUnsafe (addLogContext lCfg "simplifyDupes") InfoL msg x
                   else x

-- TODO double check that these can't have redundancies
simplifyDupes i lCfg (d:ds) = (d:) $ simplifyDupes (i+1) lCfg $ ds

-- redundantSet :: LogCfg -> Hash -> [OsPath] -> DupeSet -> Bool
redundantSet lCfg h1 fs (_,h2,_,fs') =
  let allRed = all redundant fs'
      showH1 = sbs2b8 $ unHash h1
      showH2 = sbs2b8 $ unHash h2
      msg    = showH2 <> " is redundant with " <> showH1
  in if allRed
       then logUnsafe (addLogContext lCfg "redundantSet") DebugL msg allRed
       else allRed
  where
    redundant e' = or [splitDirectories e
                       `L.isPrefixOf`
                       splitDirectories e' | e <- fs]

---------------------------- pick which dupe to keep --------------------------

-- TODO move to a util module

-- Compare paths according to my (idiosyncratic) intuition so far:
-- 1. non-hidden files first
-- 2. fewer path components first
-- 3. shorter names first
-- 4. alphabetically as usual
comparePaths :: OsPath -> OsPath -> Ordering
comparePaths a b =
  
  -- Compare as Strings, just because that's easier
  let a' = op2s a
      b' = op2s b

      startsWithDot x = not (null x) && head x == '.'
      isHiddenPath p = any startsWithDot $ LS.splitOn "/" p
      countComponents path = length (LS.splitOn "/" path)

  in case (isHiddenPath a', isHiddenPath b') of
       (True, False) -> GT
       (False, True) -> LT
       _ -> case comparing countComponents a' b' of
              EQ -> case comparing length a' b' of
                      EQ  -> compare a' b'
                      ord -> ord
              ord -> ord

-- TODO this probably needs to be OsPaths, right?
--      maybe keep the original paths, but decorate with string versions for sorting?
sortPaths :: [OsPath] -> [OsPath]
sortPaths = L.sortBy comparePaths

-------------------------- score sets for quicksorting ------------------------

{- This does a few things:
 - * adjusts the int scores from "n files in set" to "n files saved by dedup"
 - * negates scores so quicksort will put them in descending order
 - TODO should length-1 sets not be rejected?
 -}
scoreSets :: ScoreFn -> C.HashTable s Hash DupeSet -> ST s SortedDupeSets 
scoreSets scoreFn = H.foldM (
    \vs (_, v@(_,h,t,fs)) -> return $ (negate $ scoreFn v,h,t,fs):vs
  ) []
  -- TODO is removing singletons important for performance? could turn on when not vs ref set
  -- return $ if length fs > 1 then (negate $ scoreFn v,h,t,fs):vs else vs) []

type ScoreFn = DupeSet -> Int

-- | This version is for dupes vs a reference set. It's simpler because there's
-- no need to leave out one canonical version from each dupe set.
scoreSetRef :: ScoreFn
scoreSetRef (n, _, _, _) = n -- TODO is that all? lol

-- | This version is for dupes within the tree itself, which is a little more
-- complicated because we want to save (not delete) one copy from each dupe
-- group.
scoreSetSelf :: ScoreFn
scoreSetSelf (n, _, D, fs) = n - n `div` length fs
scoreSetSelf (n, _, _, _ ) = n - 1 -- TODO is this right?


------------------- filter which nodes are added to dupemaps ------------------

dupesKeepNode
  :: SearchConfig
  -> LogCfg
  -> Maybe (HashSet s)
  -> CompiledLabeledSearches
  -> [Name]
  -> Depth
  -> HashTree a
  -> ST s Bool

-- When the tree is an error, go as far as we can without inspecting it.
-- Then if needed, print an error saying we don't know whether it should be included.
-- (And don't include it, because it doesn't have a hash)
dupesKeepNode cfg lCfg _ cle ns d e@(Err {}) = do
  let info = logUnsafe (addLogContext lCfg "dupesKeepNode") InfoL
  let err  = logUnsafe (addLogContext lCfg "dupesKeepNode") ErrorL
  let wholeName = breadcrumbs2bs $ treeName e : (reverse ns)
  let excludeMsg l = "exclude node labeled '" <> l <> "' : '" <> wholeName <> "'"
  let includeMsg = "unsure whether '" <> wholeName <>
                   "' is a dupe because of prev error '" <>
                   B8.pack (show $ errMsg e) <> "'"
  let mExcludeLabel = B8.pack <$> findLabelNode cle (reverse ns) e
  return $ and
    [ maybe True (d >=) $ minDepth cfg
    , maybe True (d <=) $ maxDepth cfg
    , maybe (err includeMsg False) (\l -> info (excludeMsg l) False) mExcludeLabel
    ]

dupesKeepNode cfg lCfg mrSet cle ns d t = do
  let hash = treeHash t

  -- whether to include as a dupe because hash is in ref set
  includeHash <- case mrSet of
                   Nothing -> return False
                   Just rSet -> setContainsHash rSet hash

  let mExcludeLabel = B8.pack <$> findLabelNode cle (reverse ns) t

  let wholeName = breadcrumbs2bs $ treeName t : (reverse ns)
  let excludeMsg l = "exclude node labeled '" <> l <> "' : '" <> wholeName <> "'"
  let includeMsg   =     "dupe by ref set hash " <> prettyHash hash <> ": '" <> wholeName <> "'"
  let info = logUnsafe (addLogContext lCfg "dupesKeepNode") InfoL
  let debug = logUnsafe (addLogContext lCfg "dupesKeepNode") DebugL

  return $ and
    [ maybe True (d >=) $ minDepth cfg
    , maybe True (d <=) $ maxDepth cfg
    , maybe True (treeNBytes  t >=) $ minBytes cfg
    , maybe True (treeNBytes  t <=) $ maxBytes cfg
    , maybe True (treeNNodes  t >=) $ minFiles cfg
    , maybe True (treeNNodes  t <=) $ maxFiles cfg
    , maybe True (treeModTime t >=) $ minModtime cfg
    , maybe True (treeModTime t <=) $ maxModtime cfg
    , maybe True (treeType t `elem`) $ treeTypes cfg
    , if includeHash then debug includeMsg True else False
    -- works: , isNothing mExcludeLabel
    -- works: , maybe True (\l -> traceV verbose (excludeMsg l) False) mExcludeLabel
    , maybe True (\l -> info (excludeMsg l) False) mExcludeLabel
    ]

-- | When adding a tree to a dupemap, whether to recurse into the tree's children.
-- The tree should always be a Dir, but we don't check for that here.
dupesRecurseChildren :: SearchConfig -> Depth -> HashTree a -> Bool
dupesRecurseChildren cfg d t = and
  [ maybe True (d <) $ maxDepth cfg
  , maybe True (treeNBytes  t > ) $ minBytes cfg
  , maybe True (treeNNodes    t > ) $ minFiles cfg
  , maybe True (treeModTime t >=) $ minModtime cfg
  ]


---------------------------------- tests --------------------------------------

-- TODO property: if you dedup a list of the same dir 2+ times,
-- there should only be one big overall dupe
-- TODO property: adding to the dupe set should be idempotent
