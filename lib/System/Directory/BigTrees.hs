module System.Directory.BigTrees

  -- name
  ( Name(..)
  , NamesFwd
  , NamesRev

  , n2sbs
  , sbs2n
  , fp2n
  , fp2ns
  , n2bs
  , bs2n
  , breadcrumbs2bs
  , joinNames
  , names2bs
  , os2ns
  , op2ns

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
  , hashContents
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
  , sumNodes -- TODO rename treeNNodes?
  , readLastHashLineAndFooter
  , readTreeLines
  , getTreeSize
  , zeroModTime

  -- search
  , Search(..)
  , SearchConfig(..)
  , emptySearchConfig
  , defaultSearchConfig
  , listTreePaths
  , SearchLabel
  , LabeledSearches
  , parseLabeledSearches

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
  , simDelta
  , simDeltas
  -- , safeDelta
  -- , safeDeltas
  , assertSameTrees

  -- dupemap
  , DupeSet
  , DupeTable
  -- , DupeMap
  -- , allDupes
  , dupesByNNodes
  , pathsByHash
  , mergeDupeSets
  , printDupes
  , writeDupes
  , hWriteDupes
  -- , simplifyDupes
  -- , sortDupePaths
  )
  where

import System.Directory.BigTrees.Delta (Delta (..), assertSameTrees, diff, prettyDelta, printDeltas,
                                        simDelta, simDeltas)
import System.Directory.BigTrees.DupeMap (DupeSet, DupeTable, dupesByNNodes,
                                          mergeDupeSets, pathsByHash, printDupes,
                                          writeDupes, hWriteDupes)
import System.Directory.BigTrees.Hash (Hash (..), hashBytes, hashFile, prettyHash)
import System.Directory.BigTrees.HashLine (Depth (..), HashLine (..), ModTime (..), NBytes (..),
                                           NNodes (..), TreeType (..), linesP)
import System.Directory.BigTrees.HashSet (HashList, HashSet, Note (..), SetData (..),
                                          addNodeToHashSet, addTreeToHashSet, emptyHashSet,
                                          hashSetDataFromLine, hashSetFromList, hashSetFromTree,
                                          readHashList, readHashSet, toSortedList, writeHashList, note2bs, s2note, setContainsHash)
import System.Directory.BigTrees.HashTree (ProdTree, TestTree, readOrBuildTree)
import System.Directory.BigTrees.HashTree.Base (HashTree (..), NodeData (..), dropFileData,
                                                hashContents, renameRoot, sumNodes, treeHash,
                                                treeModTime, treeNBytes, treeName, zeroModTime)
import System.Directory.BigTrees.HashTree.Build (buildProdTree, buildTree)
import System.Directory.BigTrees.HashTree.Edit (addSubTree, rmSubTree)
import System.Directory.BigTrees.HashTree.Find (listTreePaths)
import System.Directory.BigTrees.HashTree.Read (getTreeSize, readLastHashLineAndFooter, readTree,
                                                readTreeLines)
import System.Directory.BigTrees.HashTree.Search (LabeledSearches, Search (..), SearchConfig (..),
                                                  SearchLabel, defaultSearchConfig, dropTo,
                                                  emptySearchConfig, parseLabeledSearches,
                                                  treeContainsHash, treeContainsPath)
import System.Directory.BigTrees.HashTree.Write (flattenTree, hWriteTree, printTree,
                                                 writeTestTreeDir, writeTree)
import System.Directory.BigTrees.Name (Name (..), NamesFwd, NamesRev, breadcrumbs2bs, bs2n, fp2n,
                                       fp2ns, joinNames, n2bs, n2sbs, nameP, names2bs, op2ns, os2ns,
                                       sbs2n)

import System.Directory.BigTrees.HeadFoot (headerP)
