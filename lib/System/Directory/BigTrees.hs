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
  , ProdTree
  , TestTree
  , TreeType(..)
  , ModTime(..)
  , Size(..)
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

  -- hashforest
  , HashForest(..)
  , buildForest
  , readForest
  , readTrees
  , readOrBuildTrees
  , printForest
  , writeForest
  , printForestPaths
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
import System.Directory.BigTrees.HashForest (HashForest (..), buildForest, printForest,
                                             printForestPaths, readForest, readOrBuildTrees,
                                             readTrees, writeForest)
import System.Directory.BigTrees.HashLine (TreeType (..), ModTime(..), Size(..))
import System.Directory.BigTrees.HashTree (ProdTree, TestTree, readOrBuildTree)
import System.Directory.BigTrees.HashTree.Base (HashTree (..), dropFileData, hashContents,
                                                renameRoot)
import System.Directory.BigTrees.HashTree.Build (buildProdTree, buildTree)
import System.Directory.BigTrees.HashTree.Edit (addSubTree, rmSubTree)
import System.Directory.BigTrees.HashTree.Find (Filter (..), pathMatches, printTreePaths)
import System.Directory.BigTrees.HashTree.Read (readTree)
import System.Directory.BigTrees.HashTree.Search (dropTo, treeContainsHash, treeContainsPath)
import System.Directory.BigTrees.HashTree.Write (flattenTree, printTree, writeTestTreeDir,
                                                 writeTree)
import System.Directory.BigTrees.Name (Name (..), breadcrumbs2fp, fp2n, n2fp,
                                       roundtripNameToFileName)
