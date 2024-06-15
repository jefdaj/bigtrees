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
  , readHeader
  , readLastHashLineAndFooter
  , headerP
  , linesP
  , readTreeLines
  , getTreeSize

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
  , writeHashList
  , hashSetDataFromLine

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
import System.Directory.BigTrees.HashLine (HashLine (..), ModTime (..), NBytes (..), NNodes (..),
                                           TreeType (..))
import System.Directory.BigTrees.HashSet (HashList, HashSet, Note (..), SetData (..),
                                          addNodeToHashSet, addTreeToHashSet, emptyHashSet,
                                          hashSetDataFromLine, hashSetFromList, hashSetFromTree,
                                          readHashList, toSortedList, writeHashList)
import System.Directory.BigTrees.HashTree (ProdTree, TestTree, headerP, linesP, readOrBuildTree)
import System.Directory.BigTrees.HashTree.Base (HashTree (..), NodeData (..), dropFileData,
                                                hashContents, renameRoot, sumNodes, treeHash,
                                                treeModTime, treeNBytes, treeName)
import System.Directory.BigTrees.HashTree.Build (buildProdTree, buildTree)
import System.Directory.BigTrees.HashTree.Edit (addSubTree, rmSubTree)
import System.Directory.BigTrees.HashTree.Find (Filter (..), pathMatches, printTreePaths)
import System.Directory.BigTrees.HashTree.Read (getTreeSize, readHeader, readLastHashLineAndFooter,
                                                readTree, readTreeLines)
import System.Directory.BigTrees.HashTree.Search (dropTo, treeContainsHash, treeContainsPath)
import System.Directory.BigTrees.HashTree.Write (flattenTree, hWriteTree, printTree,
                                                 writeTestTreeDir, writeTree)
import System.Directory.BigTrees.Name ( Name(..), NamesFwd, NamesRev, n2sbs, sbs2n, fp2n, fp2ns, n2bs, bs2n, breadcrumbs2bs, joinNames, names2bs, os2ns, op2ns, nameP)
 
