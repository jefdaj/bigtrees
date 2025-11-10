module System.Directory.BigTrees

  -- name
  ( Name(..)
  , NamesFwd
  , NamesRev

  , bytes2n
  , n2sbs
  , sbs2n
  , fp2n
  , fp2ns
  , n2bs
  , bs2n
  , op2bs
  , op2s
  , breadcrumbs2bs
  , joinNames
  , names2bs
  , os2ns
  , op2ns
  , b64Name
  , debugName

  , nameP

  -- hash
  , Hash(..)
  , prettyHash
  , hashBytes
  , hashFile

  -- hashline
  , HashLine(..)

  -- hashtree
  , HashTree(..)
  , NodeData(..)
  , ProdTree
  , TestTree
  , TreeType(..)
  , ModTime(..)
  , Depth(..)
  , NBytes(..)
  , NNodes(..)
  , addSubTree
  , buildTree
  , buildProdTree
  -- , deserializeTree
  , dropTo
  , dropFileData
  , flattenTree
  , hashDirContents
  -- , listAllFiles
  -- , listLostFiles
  , printTree
  , writeTree
  , hWriteTree
  , writeTestTreeDir
  , readOrBuildTree
  , readTree
  , renameRoot
  , rmSubTree
  -- , serializeTree
  , treeContainsHash
  , treeContainsPath
  , treeName
  , treeHash
  , treeNBytes
  , treeNNodes
  , readLastHashLineAndFooter
  , readTreeLines
  , getTreeSize
  , zeroModTime

  -- search
  , Search(..)
  , CompiledSearch(..)
  , SearchConfig(..)
  , emptySearchConfig
  , defaultSearchConfig
  , listTreePaths
  , SearchLabel
  , LabeledSearches
  , parseLabeledSearches
  , compileLabeledSearches

  -- hashset
  , SetData(..)
  , HashList
  , HashSet
  , Note(..)
  , emptyHashSet
  , hashSetFromTree
  , hashSetFromList
  , addTreeToHashSet
  , addNodeToHashSet
  , toSortedList
  , readHashList
  , readHashSet
  , writeHashList
  , hashSetDataFromLine
  , linesP
  , headerP
  , note2bs
  , s2note
  , setContainsHash

  -- delta
  , Delta(..)
  , diff
  , prettyDelta
  , printDeltas
  , writeDeltas
  , simDelta
  , simDeltas
  -- , safeDelta
  -- , safeDeltas
  , assertSameTrees

  -- dupemap
  , DupeMap
  , DupeSet
  , AddTreeProgress
  , DupeList
  , SortedDupeSets
  , SortedDupeLists
  , addTreeToDupeMap
  , dupesByNegScore
  , mergeDupeSets
  , pathsByHash
  , scoreSetRef
  , scoreSetSelf
  , scoreSets
  , sortPaths

  -- logging
  , LogContext
  , LogLevel (..)
  , LogCfg (..)
  , initLogger
  , cleanupLogger
  , log
  , die
  , logUnsafe
  , incLogProgressST
  )
  where

import Prelude hiding (log)

import System.Directory.BigTrees.Delta (Delta (..), assertSameTrees, diff, prettyDelta, printDeltas,
                                        writeDeltas, simDelta, simDeltas)
import System.Directory.BigTrees.DupeMap (AddTreeProgress, DupeList, DupeMap, DupeSet,
                                          SortedDupeLists, SortedDupeSets, addTreeToDupeMap,
                                          dupesByNegScore, mergeDupeSets, pathsByHash, scoreSetRef,
                                          scoreSetSelf, scoreSets, sortPaths)
import System.Directory.BigTrees.Hash (Hash (..), hashBytes, hashFile, prettyHash)
import System.Directory.BigTrees.HashLine (Depth (..), HashLine (..), ModTime (..), NBytes (..),
                                           NNodes (..), TreeType (..), linesP)
import System.Directory.BigTrees.HashSet (HashList, HashSet, Note (..), SetData (..),
                                          addNodeToHashSet, addTreeToHashSet, emptyHashSet,
                                          hashSetDataFromLine, hashSetFromList, hashSetFromTree,
                                          note2bs, readHashList, readHashSet, s2note,
                                          setContainsHash, toSortedList, writeHashList)
import System.Directory.BigTrees.HashTree (ProdTree, TestTree, readOrBuildTree)
import System.Directory.BigTrees.HashTree.Base (HashTree (..), NodeData (..), dropFileData,
                                                hashDirContents, renameRoot, treeHash, treeModTime,
                                                treeNBytes, treeNNodes, treeName, zeroModTime)
import System.Directory.BigTrees.HashTree.Build (buildProdTree, buildTree)
import System.Directory.BigTrees.HashTree.Edit (addSubTree, rmSubTree)
import System.Directory.BigTrees.HashTree.Find (listTreePaths)
import System.Directory.BigTrees.HashTree.Read (getTreeSize, readLastHashLineAndFooter, readTree,
                                                readTreeLines)
import System.Directory.BigTrees.HashTree.Search (CompiledLabeledSearches, CompiledSearch (..),
                                                  LabeledSearches, Search (..), SearchConfig (..),
                                                  SearchLabel, compileLabeledSearches,
                                                  defaultSearchConfig, dropTo, emptySearchConfig,
                                                  parseLabeledSearches, treeContainsHash,
                                                  treeContainsPath)
import System.Directory.BigTrees.HashTree.Write (flattenTree, hWriteTree, printTree,
                                                 writeTestTreeDir, writeTree)
import System.Directory.BigTrees.Name (Name (..), NamesFwd, NamesRev, b64Name, breadcrumbs2bs, bs2n,
                                       bytes2n, debugName, fp2n, fp2ns, joinNames, n2bs, n2sbs,
                                       nameP, names2bs, op2bs, op2ns, op2s, os2ns, sbs2n)

import System.Directory.BigTrees.HeadFoot (headerP)

import System.Directory.BigTrees.Logging (LogCfg (..), LogContext, LogLevel (..), addLogContext,
                                          cleanupLogger, die, incLogProgressST, initLogger, log,
                                          logUnsafe)
