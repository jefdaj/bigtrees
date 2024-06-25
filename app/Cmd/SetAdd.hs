module Cmd.SetAdd (cmdSetAdd) where

import Config (AppConfig (..), log)
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
                                  readLastHashLineAndFooter, readOrBuildTree, readTreeLines,
                                  sumNodes, toSortedList, writeHashList, s2note)
import System.Directory.BigTrees.HashSet (emptyHashSet)
import qualified System.Directory.OsPath as SDO
import System.IO (IOMode (..), withFile)
import System.OsPath (OsPath)
import Text.Pretty.Simple (pPrint)

readTreeHashList :: AppConfig -> Maybe Note -> OsPath -> IO HashList
readTreeHashList cfg mn path = do
  ls <- readTreeLines path
  let hl = mapMaybe (hashSetDataFromLine mn) ls
  log cfg $ "adding hashes from " ++ show path
  return hl

readHashListIO :: AppConfig -> OsPath -> IO HashList
readHashListIO cfg path = do
  log cfg $ "adding hashes from " ++ show path
  eHL <- readHashList path
  case eHL of
    Left msg -> error $ "failed to read " ++ show path
    Right hl -> return hl

cmdSetAdd :: AppConfig -> OsPath -> Maybe String -> [OsPath] -> IO ()
cmdSetAdd _ _ _ [] = return () -- Docopt should prevent this, but just in case
cmdSetAdd cfg setPath mNoteStr treePaths = do

  -- TODO can this conflict with writing the file later? (length should force it)
  exists <- SDO.doesPathExist setPath
  before <- if exists
              then do
                hl <- readHashListIO cfg setPath
                log cfg $
                  "initial " ++ show setPath ++
                  " contains " ++ show (length hl) ++
                  " hashes"
                return hl
              else do
                log cfg $ show setPath ++ " does not exist yet"
                return []

  -- the actual set should be smaller (assuming some dupes),
  -- but this will prevent having to do any resizing
  maxSetSize <- (sum . catMaybes) <$> mapM getTreeSize treePaths
  let maxSetSize' = maxSetSize + length before
  log cfg $ "max expected set size: " ++ show maxSetSize'

  let mNote = s2note <$> mNoteStr

  -- create empty hashset and fold over the trees to add elements
  hl <- concat <$> mapM (readTreeHashList cfg mNote) treePaths
  let afterL = toSortedList $ do
                 s <- emptyHashSet maxSetSize'
                 forM_ (before ++ hl) $ uncurry (addNodeToHashSet s)
                 return s

  writeHashList setPath afterL
  log cfg $ "final " ++ show setPath ++ " contains " ++ show (length afterL) ++ " hashes"
