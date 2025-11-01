module Cmd.SetAdd (cmdSetAdd) where

import Config (AppConfig (..))
import Control.DeepSeq (force)
import Control.Monad (foldM, forM, forM_)
import Data.Attoparsec.ByteString.Char8 (char, parseOnly)
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashTable.Class as H
import Data.Maybe (catMaybes, mapMaybe)
import Prelude hiding (log)
import System.Directory.BigTrees (HashLine (..), HashList, Note (..), addNodeToHashSet,
                                  addTreeToHashSet, getTreeSize, hashSetDataFromLine,
                                  hashSetFromList, headerP, linesP, readHashList,
                                  readLastHashLineAndFooter, readOrBuildTree, readTreeLines, s2note,
                                  toSortedList, treeNNodes, writeHashList)
import System.Directory.BigTrees.HashSet (emptyHashSet)
import System.Directory.BigTrees.Logging (LogCfg)
import qualified System.Directory.OsPath as SDO
import System.IO (IOMode (..), withFile)
import System.OsPath (OsPath)
import Text.Pretty.Simple (pPrint)

readTreeHashList :: AppConfig -> LogCfg -> Maybe Note -> OsPath -> IO HashList
readTreeHashList cfg lCfg mn path = do
  ls <- readTreeLines lCfg path
  let hl = mapMaybe (hashSetDataFromLine mn) ls
  -- log cfg $ "adding hashes from " ++ show path
  return hl

readHashListIO :: AppConfig -> LogCfg -> OsPath -> IO HashList
readHashListIO cfg lCfg path = do
  -- log cfg $ "adding hashes from " ++ show path
  readHashList lCfg path

cmdSetAdd :: AppConfig -> LogCfg -> OsPath -> Maybe String -> [OsPath] -> IO ()
cmdSetAdd _ _ _ _ [] = return () -- Docopt should prevent this, but just in case
cmdSetAdd cfg lCfg setPath mNoteStr treePaths = do

  -- TODO can this conflict with writing the file later? (length should force it)
  exists <- SDO.doesPathExist setPath
  before <- if exists
              then do
                hl <- readHashListIO cfg lCfg setPath
                -- log cfg $
                --   "initial " ++ show setPath ++
                --   " contains " ++ show (length hl) ++
                --   " hashes"
                return hl
              else do
                -- log cfg $ show setPath ++ " does not exist yet"
                return []

  -- the actual set should be smaller (assuming some dupes),
  -- but this will prevent having to do any resizing
  maxSetSize <- (sum . catMaybes) <$> mapM getTreeSize treePaths
  let maxSetSize' = maxSetSize + length before
  -- log cfg $ "max expected set size: " ++ show maxSetSize'

  -- log cfg $ "note: " ++ show mNoteStr
  let mNote = s2note <$> mNoteStr

  -- create empty hashset and fold over the trees to add elements
  hl <- concat <$> mapM (readTreeHashList cfg lCfg mNote) treePaths
  let afterL = toSortedList $ do
                 s <- emptyHashSet maxSetSize'
                 forM_ (before ++ hl) $ uncurry (addNodeToHashSet s)
                 return s

  writeHashList setPath afterL
  -- log cfg $ "final " ++ show setPath ++ " contains " ++ show (length afterL) ++ " hashes"
