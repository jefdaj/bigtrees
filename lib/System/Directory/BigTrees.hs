module System.Directory.BigTrees

  -- name
  ( Name(..)

  -- hash
  , Hash(..)
  , prettyHash
  , hashBytes
  , hashFile

  -- hashtree
  , HashTree(..)
  , ProdTree
  , TestTree
  , TreeType(..)
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
  , readOrBuildTree
  , readTree
  , renameRoot
  , rmSubTree
  -- , serializeTree
  , treeContainsHash
  , treeContainsPath

  -- hashforest
  , HashForest(..)
  , buildForest
  , readForest
  , readTrees
  , readOrBuildTrees
  , printForest
  , writeForest
  -- , serializeForest
  -- , deserializeForest

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
  , dupesByNFiles
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
import System.Directory.BigTrees.DupeMap (DupeMap, DupeSet, allDupes, dupesByNFiles, listAllFiles,
                                          listLostFiles, mergeDupeSets, pathsByHash, printDupes,
                                          writeDupes)
import System.Directory.BigTrees.Hash (Hash (..), hashBytes, hashFile, prettyHash)
import System.Directory.BigTrees.HashForest (HashForest (..), buildForest, printForest, readForest,
                                             readOrBuildTrees, readTrees,
                                             writeForest)
import System.Directory.BigTrees.Name (Name (..))
import System.Directory.BigTrees.HashLine (TreeType (..))
import System.Directory.BigTrees.HashTree (readOrBuildTree, ProdTree, TestTree)
import System.Directory.BigTrees.HashTree.Base (HashTree (..), hashContents, renameRoot, dropFileData)
import System.Directory.BigTrees.HashTree.Build (buildProdTree, buildTree)
import System.Directory.BigTrees.HashTree.Edit (addSubTree, rmSubTree)
import System.Directory.BigTrees.HashTree.Read (readTree)
import System.Directory.BigTrees.HashTree.Search (dropTo, treeContainsHash, treeContainsPath)
import System.Directory.BigTrees.HashTree.Write (flattenTree, printTree, writeTree)
