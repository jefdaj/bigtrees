module Cmd.SetAdd (cmdSetAdd) where

import Text.Pretty.Simple (pPrint)
import Prelude hiding (log)
import Config (Config(..), log)
import Control.Monad (forM, forM_, foldM)
import System.Directory.BigTrees (readOrBuildTree, readHashList, writeHashList, hashSetFromList, addTreeToHashSet, toSortedList, HashList, Note(..), sumNodes, HashLine(..), readLastHashLineAndFooter, headerP, linesP, hashSetDataFromLine, addNodeToHashSet, readTreeLines, getTreeSize)
import System.Directory.BigTrees.HashSet (emptyHashSet)
import Control.DeepSeq (force)
import qualified System.Directory as SD
import qualified Data.Text as T
import Data.Maybe (catMaybes)
import qualified Data.HashTable.Class as H
import Data.Attoparsec.ByteString.Char8 (parseOnly, char)
import System.IO (withFile, IOMode(..))
import qualified Data.ByteString.Char8 as B8

readTreeHashList :: Config -> Maybe Note -> FilePath -> IO HashList
readTreeHashList cfg mn path = do
  ls <- readTreeLines path
  let hl = catMaybes $ map (hashSetDataFromLine mn) ls
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
                 forM_ (before ++ hl) $ \(h, sd) -> addNodeToHashSet s h sd
                 return s

  writeHashList setPath afterL
  log cfg $ "final '" ++ setPath ++ "' contains " ++ show (length afterL) ++ " hashes"
