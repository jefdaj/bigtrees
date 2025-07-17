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
  , SortedDupeLists
  , SortedDupeSets
  , ExplainFn
  , AddTreeProgress
  , addTreeToDupeMap
  , dupesByNegScore
  , renderDupesSuggestions
  , renderDupesRsyncFilter
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
import System.Directory.BigTrees.Hash (Hash, unHash)
import System.Directory.BigTrees.Name (Name (..), n2op, op2ns, breadcrumbs2bs)
import System.Directory.BigTrees.HashLine (Depth (..), NNodes (..), TreeType (..))
import System.Directory.BigTrees.HashTree (HashTree (..), NodeData (..),
                                           ProdTree, treeType, treeHash, treeModTime, treeNNodes, treeNBytes,
                                           treeName, SearchConfig (..))
import System.Directory.BigTrees.Logging (LogFn, LogLevel (..), LogContext, logMaybeUnsafe, die)
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
-- incTreeProgress :: Maybe LogFn -> AddTreeProgress -> ST s AddTreeProgress
-- incTreeProgress mLog (AddTreeProgress nSoFar nTotal) = AddTreeProgress (nSoFar + 1) nTotal

------------------------------- create dupemaps -------------------------------

-- TODO unify with incLogProgressST in Logging
incAddTreeProgress :: Maybe LogFn -> STRef s AddTreeProgress -> ST s ()
incAddTreeProgress mLog progressRef = do
  n <- readSTRef progressRef
  let n'@(AddTreeProgress nNodes) = n + 1
      msg = "added " <> B8.pack (show nNodes) <> " nodes"
      action = writeSTRef progressRef n'
  -- log only every 1000 nodes
  -- TODO make this configurable or auto-adjust?
  if nNodes `mod` 1000 == 0
     then logMaybeUnsafe mLog InfoL "addTreeToDupeMap" msg action
     else action
    

-- TODO what about if we guess the approximate size first?
-- TODO what about if we make it from the serialized hashes instead of a tree?
pathsByHash
  :: SearchConfig -> Maybe LogFn -> Maybe (HashSet s) -> CompiledLabeledSearches
  -> HashTree a -> ST s (DupeMap s)
pathsByHash cfg mLog mrSet cle tree = do
  -- let (NNodes n) = treeNNodes tree TODO does this force evaluation??
      -- info msg = logMaybeUnsafe mLog InfoL "pathsByHash" msg $ return ()
  -- TODO is it more wasteful to allocate it too large like this, or to expand it?
  dm <- H.newSized 1000 -- n
  -- info $ "adding " <> B8.pack (show n) <> " nodes to hashmap" -- TODO inside addTreeToDupeMap?
  addTreeToDupeMap cfg mLog mrSet cle dm tree
  -- TODO try putting it back and compare overall speed
  -- H.mapM_ (\(k,_) -> H.mutate dm k removeNonDupes) dm
  return dm

-- inserts all nodes from a tree into an existing dupemap
-- TODO The empty string (mempty) behaves right, right? (disappears)
addTreeToDupeMap
  :: SearchConfig -> Maybe LogFn -> Maybe (HashSet s) -> CompiledLabeledSearches
  -> DupeMap s -> HashTree a -> ST s ()
addTreeToDupeMap cfg mLog mrSet cle dm t = do
  pRef <- newSTRef $ AddTreeProgress 0
  addTreeToDupeMap' cfg mLog mrSet cle dm mempty (Depth 0) pRef t

-- same, but start from a given root path
-- TODO NamesFwd or NamesRev instead of OsPath?
addTreeToDupeMap'
  :: SearchConfig
  -> Maybe LogFn
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
addTreeToDupeMap' cfg mLog mrSet cle dm dir _ pr l@(Link {nodeData=NodeData {hash=h}}) = do
  keepNode <- dupesKeepNode cfg mLog mrSet cle (op2ns dir) l
  when keepNode $
    insertDupeSet cfg mLog dm (treeHash l) (1, h, treeType l, S.singleton $ dir </> n2op (treeName l)) pr

addTreeToDupeMap'
  cfg mLog mrSet cle dm dir _ pr
  f@(File {nodeData=(NodeData{name=Name n, hash=h})}) = do
    keepNode <- dupesKeepNode cfg mLog mrSet cle (op2ns dir) f
    when keepNode $
      insertDupeSet cfg mLog dm h (1, h, F, S.singleton $ dir </> n) pr

addTreeToDupeMap'
  cfg mLog mrSet cle dm dir depth pr
  d@(Dir {nodeData=(NodeData{name=Name n, hash=h}), dirContents=cs, nNodes=(NNodes fs)}) = do
    keepNode <- dupesKeepNode cfg mLog mrSet cle (op2ns dir) d
    let recurse = dupesRecurseChildren cfg depth d
    when keepNode $ do
      insertDupeSet cfg mLog dm h (fs, h, D, S.singleton $ dir </> n) pr
      -- TODO is there any situation where we want to NOT keep the current node, but still recurse?
      when recurse $
        mapM_ (addTreeToDupeMap' cfg mLog mrSet cle dm (dir </> n) (depth+1) pr) cs

-- inserts one node into an existing dupemap
-- TODO any reason not to pass the tree here instead? then all the "keepNode" stuff can go here
insertDupeSet :: SearchConfig -> Maybe LogFn -> DupeMap s -> Hash -> DupeSet -> STRef s AddTreeProgress -> ST s ()
insertDupeSet cfg mLog dm h d2 pRef = do
  let debug  = logMaybeUnsafe mLog DebugL "insertDupeSet"
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
      in debug msg $ H.insert dm h $ mergeDupeSets mLog d1 d2
  incAddTreeProgress mLog pRef

-- TODO is DupeSet a Monoid? or not, because there are some you can't merge?
mergeDupeSets :: Maybe LogFn -> DupeSet -> DupeSet -> DupeSet
mergeDupeSets mLog (n1, h1, t1, l1) d2@(n2, h2, t2, l2) = (n1 + n2, h, t, S.union l1 l2)
  where
    die' = die mLog "mergeDupeSets"
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
dupesByNegScore :: Maybe LogFn -> ScoreFn -> DupeMap s -> ST s SortedDupeLists
dupesByNegScore mLog scoreFn dm = do
  let debug = logMaybeUnsafe mLog DebugL "dupesByNegScore"
  sets <- debug "scoring sets" <$> scoreSets scoreFn dm -- TODO separate scoring for ref set than within same tree
  let unsorted = debug "creating DupeSetVec" $ A.fromList A.Par $ deepseq sets sets :: DupeSetVec
      sorted   = debug "quicksorting DupeSetVec" $ A.quicksort $ A.compute $ deepseq unsorted unsorted :: DupeSetVec
      sortedL  = debug "converting DupeSetVec back to list" $ A.toList $ deepseq sorted sorted
      fixElem (n, h, t, fs) = (negate n, h, t, L.sort $ S.toList fs) -- TODO n before h?
      fixed    = Prelude.map fixElem $ deepseq sortedL sortedL
      simple = debug "simplifying dupes" $ simplifyDupes 1 mLog $ deepseq fixed fixed -- TODO helps?
  return simple

{- Assumes a pre-sorted list of lists.
 - Removes lists whose elements are all inside elements of the first list.
 - For example if the first is dir1, dir2, dir3
 - and the next is dir1/file.txt, dir2/file.txt, dir3/file.txt
 - ... then the second set is redundant and confusing to show.
 -}
simplifyDupes :: Int -> Maybe LogFn -> SortedDupeLists -> SortedDupeLists

simplifyDupes _ _ [ ] = [ ]
simplifyDupes _ _ [d] = [d]

simplifyDupes i mLog (d@(n,h,D,fs):ds) = info msg $ (d:) $ simplifyDupes (i+1) mLog $ ds'
  where
    showH = sbs2b8 $ unHash h
    showI = B8.pack $ show i
    showN = B8.pack $ show n
    showR = B8.pack $ show nRemain
    msg = "iteration " <> showI <>
          " drop " <> showN <>
          " sets redundant with " <> showH <> "; " <> showR <>
	  " sets remain to process"
    ds' = filter (not . redundantSet) ds
    nRemain = length ds'
    nSaved = length ds - nRemain
    info msg x = if nSaved > 0 then logMaybeUnsafe mLog InfoL "simplifyDupes" msg x else x
    redundantSet (_,_,_,fs') = all redundant fs'
    redundant e' = or [splitDirectories e
                       `L.isPrefixOf`
                       splitDirectories e' | e <- fs]

-- TODO double check that these can't have redundancies
simplifyDupes i mLog (d:ds) = (d:) $ simplifyDupes (i+1) mLog $ ds

---------------------------- pick which dupe to keep --------------------------

-- TODO move to a util module

-- Compare paths according to my (idiosyncratic) intuition so far:
-- 1. non-hidden files first
-- 2. fewer path components first
-- 3. shorter names first
-- 4. alphabetically as usual
comparePaths :: String -> String -> Ordering
comparePaths a b =

  let startsWithDot x = not (null x) && head x == '.'
      isHiddenPath p = any startsWithDot $ LS.splitOn "/" p
      countComponents path = length (LS.splitOn "/" path)

  in case (isHiddenPath a, isHiddenPath b) of
       (True, False) -> GT
       (False, True) -> LT
       _ -> case comparing countComponents a b of
              EQ -> case comparing length a b of
                      EQ  -> compare a b
                      ord -> ord
              ord -> ord

sortPaths :: [String] -> [String]
sortPaths = L.sortBy comparePaths

-------------------------- score sets for quicksorting ------------------------

{- This does a few things:
 - * removes singleton sets (no duplicates)
 - * adjusts the int scores from "n files in set" to "n files saved by dedup"
 - * negates scores so quicksort will put them in descending order
 - TODO should length-1 sets not be rejected?
 -}
scoreSets :: ScoreFn -> C.HashTable s Hash DupeSet -> ST s SortedDupeSets 
scoreSets scoreFn = H.foldM (\vs (_, v@(_,h,t,fs)) ->
  return $ if length fs > 1 then (negate $ scoreFn v,h,t,fs):vs else vs) []

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


-------------------------------- write output ---------------------------------

-- TODO factor explainFn out here?
hWriteDupes :: SearchConfig -> ExplainFn -> Bool -> Handle -> SortedDupeLists -> IO ()
hWriteDupes cfg explainFn keepOneDupe hdl groups = do
  msg <- explainFn keepOneDupe (maxDepth cfg) groups
  B8.hPutStr hdl msg

type ExplainFn = Bool -> Maybe Depth -> SortedDupeLists -> IO B8.ByteString

renderDupesSuggestions :: ExplainFn
renderDupesSuggestions keepOne md ls = do
  body <- mapM excludeLines ls
  return $ B8.unlines $ fileHeader : body
  where

    fileHeader = B8.pack $
      "# This is the default 'suggestions' output format.\n\
      \# It just suggests what you might delete manually yourself.\n"
      ++ if keepOne then "" else
      "\n\
      \# Since you're deduping vs a reference set, the suggestion is to\n\
      \# delete ALL these dupes, assuming you have another copy wherever you got\n\
      \# the reference set from.\n"

    depthWarning Nothing  = ""
    depthWarning (Just (Depth d)) =
      " (up to " `B8.append` B8.pack (show d) `B8.append` " levels deep)"

    excludeLines :: DupeList -> IO B8.ByteString
    excludeLines (n, _, t, paths) = do
      paths' <- mapM decodeFS paths -- TODO is decoding necessary, even to write a script?
      return $ B8.unlines
             $ groupHeader t n (length paths)
             : (map B8.pack $ sortPaths paths')

    -- TODO is n the number *saved*, or total number of dupes?
    groupHeader :: TreeType -> Int -> Int -> B8.ByteString
    groupHeader E _ _ = "" -- TODO is that a good idea?
    groupHeader D nSaved nDirs = B8.intercalate " "
      [ "# You could save", B8.pack (show nSaved)
      , "inodes by deleting all but one of these", B8.pack (show nDirs)
      , B8.append "duplicate directories" (depthWarning md)
      ]
    groupHeader F nSaved nFiles = B8.intercalate " "
      [ "# You could delete", B8.pack (show $ nFiles - 1), "of these", B8.pack (show nFiles)
      , "duplicate files", depthWarning md
      ]
    groupHeader _ nSaved nLinks = B8.intercalate " "
      [ "# You could delete", B8.pack (show $ nLinks - 1), "of these"  , B8.pack (show nLinks)
      , "duplicate links", depthWarning md
      ]

replaceTopDirWithSlash :: String -> String
replaceTopDirWithSlash path = '/' : L.intercalate "/" pathTail
  where
    comps = LS.splitOn "/" path
    pathTail = if null comps then [] else tail comps

-- escapeRsyncExcludeSpecialChars :: String -> String
-- escapeRsyncExcludeSpecialChars input = concatMap escapeChar input
--   where
--     specialChars = "*?#\!()" :: String
--     escapeChar c
--       | c `L.elem` specialChars = '\\' : [c]
--       | otherwise  = [c]

escapeRsyncExcludeFromPath2 :: String -> String
escapeRsyncExcludeFromPath2 path = if wildcardMode then escaped else path
  where

    -- and if the path starts with # that needs to be escaped to prevent being
    -- treated as a comment
    -- TODO but that never happens here because we prepend /, right?

    -- if path has one of these, rsync will treat it as a pattern;
    -- if not, everything is matched literally and \ etc will break it!
    wildcardMode = any (`L.elem` path) wildcardTriggerChars
    wildcardTriggerChars = "*?[" :: String

    -- Once wildcard mode is triggered, these chars need escaping:
    -- TODO verify each one!
    escaped = concatMap escapeChar path
    -- specialChars = "*?#\\!()" :: String
    specialChars = "*?[\\" :: String
    escapeChar c
      | c `L.elem` specialChars = '\\' : [c]
      | otherwise  = [c]


renderDupesRsyncFilter :: ExplainFn
renderDupesRsyncFilter keepOne md ls = do
  body <- mapM groupDupes ls
  return $ B8.unlines $ fileHeader : body
  where

    fileHeader = B8.pack $
      "# This is the 'rsync-filter-file' output format.\n\
      \# You can use it to tell rsync all the files *not* to copy.\n\
      \# Example rsync command:\n\
      \#\n\
      \# rsync -arv SRCDIR/ DSTDIR/ --filter 'merge THISFILE'\n\
      \#\n\
      \# Where SRCDIR is your originally scanned folder with duplicates,\n\
      \# DSTDIR is the new, non-duplicated copy you'll be making,\n\
      \# and THISFILE is where you saved the output of this command.\n"
      ++ (if keepOne then "" else
      "#\n\
      \# WARNING!\n\
      \# Since you're deduping vs a reference set, ALL dupes will be listed\n\
      \# in the exclude file. The assumption is that you already have another copy\n\
      \# saved somewhere else, and that was used to generate the reference set.\n")
      ++
      "#\n\
      \# Note that the trailing slashes in the rsync command above and the\n\
      \# leading slashes in each filename below are important.\n\
      \#\n\
      \# You might want to try the command with --dry-run at the end first\n\
      \# to make sure it does what you expected!\n"

    depthWarning Nothing  = ""
    depthWarning (Just (Depth d)) =
      " (up to " `B8.append` B8.pack (show d) `B8.append` " levels deep)"

    groupDupes :: DupeList -> IO B8.ByteString
    groupDupes (n, _, t, paths) = do
      paths' <- mapM decodeFS paths -- TODO is decoding necessary, even to write a script?
      let paths''   = sortPaths $ map (escapeRsyncExcludeFromPath2 . replaceTopDirWithSlash) paths'
          paths'''  = if t == D then map (++ "/") paths'' else paths''
          paths'''' = if not keepOne
                       then map ("- " ++) $ paths'''
                       else ("+ " ++ head paths'''):(map ("- " ++) $ tail paths''')
      return $ B8.unlines $ groupHeader t n (length paths) : map B8.pack paths''''

    nSkip ds = B8.pack $ show $ if keepOne then ds - 1 else ds

    exclude ds = if not keepOne then "exclude" else
                   if ds > 2 then "exclude all but one of"
                     else "exclude one of"

    plural :: Int -> B8.ByteString -> B8.ByteString
    plural n thing = if n > 1 then thing `B8.append` "s" else thing

    -- TODO don't mention inodes unless it's a dir, so separate fn for that
    explain :: Int -> Int -> B8.ByteString -> B8.ByteString
    explain nSaved nThings thing = B8.intercalate " "
      [ "#", exclude nThings , "these", B8.pack $ show nThings , "duplicate"
      , thing `B8.append` "s," , "saving", B8.pack $ show nSaved
      , (plural nSaved "inode") `B8.append` ":"
      ]

    -- TODO is n the number *saved*, or total number of dupes?
    groupHeader :: TreeType -> Int -> Int -> B8.ByteString
    groupHeader E _ _  = "" -- TODO is that a good idea?
    groupHeader D nSaved nDirs  = explain nSaved nDirs "folder"
    groupHeader F nSaved nFiles = explain nSaved nFiles "file"
    groupHeader _ nSaved nLinks = explain nSaved nLinks "link"

------------------- filter which nodes are added to dupemaps ------------------

dupesKeepNode :: SearchConfig -> Maybe LogFn -> Maybe (HashSet s) -> CompiledLabeledSearches -> [Name] -> HashTree a -> ST s Bool
dupesKeepNode _ _ _ _ _ (Err {}) = return False -- TODO is this how we should handle them?
dupesKeepNode cfg mLog mrSet cle ns t = do
  includeHash <- case mrSet of
                   Nothing -> return True
                   Just rSet -> setContainsHash rSet $ treeHash t

  let mExcludeLabel = B8.pack <$> findLabelNode cle (reverse ns) t

  let wholeName = breadcrumbs2bs $ treeName t : (reverse ns)
  let excludeMsg l = "exclude node labeled '" <> l <> "' : '" <> wholeName <> "'"
  let info = logMaybeUnsafe mLog InfoL "dupesKeepNode"

  return $ and
    [ maybe True (treeNBytes  t >=) $ minBytes cfg
    , maybe True (treeNBytes  t <=) $ maxBytes cfg
    , maybe True (treeNNodes  t >=) $ minFiles cfg
    , maybe True (treeNNodes  t <=) $ maxFiles cfg
    , maybe True (treeModTime t >=) $ minModtime cfg
    , maybe True (treeModTime t <=) $ maxModtime cfg
    , maybe True (treeType t `elem`) $ treeTypes cfg
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
