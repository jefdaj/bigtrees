module Cmd.SetAdd where

import Text.Pretty.Simple (pPrint)
import Config (Config(..))
import Control.Monad (forM, foldM)
import System.Directory.BigTrees (readOrBuildTree, readHashList, writeHashList, hashSetFromList, addTreeToHashSet, toSortedList, HashList, Note(..))
import Control.Monad.ST.Strict (runST)
import Control.DeepSeq (force)
import qualified System.Directory as SD
import Data.Text as T

-- bigtrees [-v] set-add -s <set> [-n <note>] <tree>...
cmdSetAdd :: Config -> FilePath -> Maybe String -> [FilePath] -> IO ()
cmdSetAdd cfg setPath mNoteStr treePaths = do
  let mNote = (Note . T.pack) <$> mNoteStr
  exists <- SD.doesPathExist setPath
  -- force ensures read is strict here so it doesn't conflict with
  -- writing to the same file below
  eBefore <- fmap force $ if exists
               then readHashList setPath
               else return $ Right []
  case eBefore of
    Left msg -> error msg
    Right before -> do
      -- TODO does doing the trees one at a time fix the RAM leak?
      afterL <- foldM (readAndAddTree cfg mNote) before treePaths
      writeHashList setPath afterL

readAndAddTree :: Config -> Maybe Note -> HashList -> FilePath -> IO HashList
readAndAddTree cfg mNote before path = do
  tree <- readOrBuildTree (verbose cfg) (maxdepth cfg) (exclude cfg) path
  -- TODO is it weird that toSortedList includes runST?
  return $ toSortedList $ do
    after <- hashSetFromList before
    addTreeToHashSet mNote after tree
    return after
