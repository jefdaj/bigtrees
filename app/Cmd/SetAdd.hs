module Cmd.SetAdd (cmdSetAdd) where

import Config (Config (..), log)
import Control.DeepSeq (force)
import Control.Monad (foldM, forM, forM_)
import Data.Attoparsec.ByteString.Char8 (char, parseOnly)
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashTable.Class as H
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Text as T
import Prelude hiding (log)
import qualified System.Directory as SD
import System.Directory.BigTrees (HashLine (..), HashList, Note (..), addNodeToHashSet,
                                  addTreeToHashSet, getTreeSize, hashSetDataFromLine,
                                  hashSetFromList, headerP, linesP, readHashList,
                                  readLastHashLineAndFooter, readOrBuildTree, readTreeLines,
                                  sumNodes, toSortedList, writeHashList)
import System.Directory.BigTrees.HashSet (emptyHashSet)
import System.IO (IOMode (..), withFile)
import Text.Pretty.Simple (pPrint)

readTreeHashList :: Config -> Maybe Note -> FilePath -> IO HashList
readTreeHashList cfg mn path = do
  ls <- readTreeLines path
  let hl = mapMaybe (hashSetDataFromLine mn) ls
  log cfg $ "adding hashes from '" ++ path ++ "'"
  return hl

readHashListIO :: Config -> FilePath -> IO HashList
readHashListIO cfg path = do
  log cfg $ "adding hashes from '" ++ path ++ "'"
  eHL <- readHashList path
  case eHL of
    Left msg -> error $ "failed to read '" ++ path ++ "'"
    Right hl -> return hl

cmdSetAdd :: Config -> FilePath -> Maybe String -> [FilePath] -> IO ()
cmdSetAdd _ _ _ [] = return () -- Docopt should prevent this, but just in case
cmdSetAdd cfg setPath mNoteStr treePaths = do

  -- TODO can this conflict with writing the file later? (length should force it)
  exists <- SD.doesPathExist setPath
  before <- if exists
              then do
                hl <- readHashListIO cfg setPath
                log cfg $
                  "initial '" ++ setPath ++
                  "' contains " ++ show (length hl) ++
                  " hashes"
                return hl
              else do
                log cfg $ "'" ++ setPath ++ "' does not exist yet"
                return []

  -- the actual set should be smaller (assuming some dupes),
  -- but this will prevent having to do any resizing
  maxSetSize <- (sum . catMaybes) <$> mapM getTreeSize treePaths
  let maxSetSize' = maxSetSize + length before
  log cfg $ "max expected set size: " ++ show maxSetSize'

  let mNote = (Note . T.pack) <$> mNoteStr

  -- create empty hashset and fold over the trees to add elements
  hl <- concat <$> mapM (readTreeHashList cfg mNote) treePaths
  let afterL = toSortedList $ do
                 s <- emptyHashSet maxSetSize'
                 forM_ (before ++ hl) $ uncurry (addNodeToHashSet s)
                 return s

  writeHashList setPath afterL
  log cfg $ "final '" ++ setPath ++ "' contains " ++ show (length afterL) ++ " hashes"
