module System.Directory.BigTrees

  -- name
  ( Name(..)
  , n2fp
  , fp2n
  , breadcrumbs2fp
  , roundtripNameToFileName

  -- hash
  , Hash(..)
  , prettyHash
  , hashBytes
  , hashFile

  -- hashtree
  , HashTree(..)
  , NodeData(..)
  , ProdTree
  , TestTree
  , TreeType(..)
  , ModTime(..)
  , NBytes(..)
  , addSubTree
  , buildTree
  , buildProdTree
  -- , deserializeTree
  , dropTo
  , dropFileData
  , flattenTree
  , hashContents
  , listAllFiles
  , listLostFiles
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
  , printTreePaths
  , Filter(..)
  , pathMatches
  , treeName
  , treeHash
  , treeNBytes
  , sumNodes -- TODO rename treeNNodes?

  -- hashset
  , SetData(..)
  , HashList
  , HashSet
  , hashSetFromTree
  , hashSetFromList
  , addTreeToHashSet
  , toSortedList
  , readHashList
  , writeHashList

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
  , DupeMap
  , allDupes
  , dupesByNNodes
  , pathsByHash
  , mergeDupeSets
  , printDupes
  , writeDupes
  -- , simplifyDupes
  -- , sortDupePaths
  )
  where

import System.Directory.BigTrees.Delta (Delta (..), assertSameTrees, diff, prettyDelta, printDeltas,
                                        simDelta, simDeltas)
import System.Directory.BigTrees.DupeMap (DupeMap, DupeSet, allDupes, dupesByNNodes, listAllFiles,
                                          listLostFiles, mergeDupeSets, pathsByHash, printDupes,
                                          writeDupes)
import System.Directory.BigTrees.Hash (Hash (..), hashBytes, hashFile, prettyHash)
import System.Directory.BigTrees.HashLine (ModTime (..), NBytes (..), TreeType (..))
import System.Directory.BigTrees.HashTree (ProdTree, TestTree, readOrBuildTree)
import System.Directory.BigTrees.HashTree.Base (HashTree (..), NodeData (..), dropFileData,
                                                hashContents, renameRoot, treeName, treeHash, treeNBytes, treeModTime, sumNodes)
import System.Directory.BigTrees.HashTree.Build (buildProdTree, buildTree)
import System.Directory.BigTrees.HashTree.Edit (addSubTree, rmSubTree)
import System.Directory.BigTrees.HashTree.Find (Filter (..), pathMatches, printTreePaths)
import System.Directory.BigTrees.HashTree.Read (readTree, parseHeader, parseFooter)
import System.Directory.BigTrees.HashTree.Search (dropTo, treeContainsHash, treeContainsPath)
import System.Directory.BigTrees.HashTree.Write (flattenTree, hWriteTree, printTree,
                                                 writeTestTreeDir, writeTree)
import System.Directory.BigTrees.Name (Name (..), breadcrumbs2fp, fp2n, n2fp,
                                       roundtripNameToFileName)
import System.Directory.BigTrees.HashSet (HashSet, SetData(..), HashList, hashSetFromTree, hashSetFromList, addTreeToHashSet, toSortedList, readHashList, writeHashList)
