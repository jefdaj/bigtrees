module Cmd.SetAdd where

import Text.Pretty.Simple (pPrint)
import Prelude hiding (log)
import Config (Config(..), log)
import Control.Monad (forM, foldM)
import System.Directory.BigTrees (readOrBuildTree, readHashList, writeHashList, hashSetFromList, addTreeToHashSet, toSortedList, HashList, Note(..), NNodes(..), sumNodes, HashLine(..), readLastHashLineAndFooter)
import Control.DeepSeq (force)
import qualified System.Directory as SD
import qualified Data.Text as T
import Data.Maybe (catMaybes)

--- old version: works but slow + high mem usage ---

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
      log cfg $ "initial '" ++ setPath ++ "' contains " ++ show (length before) ++ " hashes"
      afterL <- foldM (force . readAndAddTree cfg mNote) before treePaths
      log cfg $ "final '" ++ setPath ++ "' contains " ++ show (length afterL) ++ " hashes"
      writeHashList setPath afterL

readAndAddTree :: Config -> Maybe Note -> HashList -> FilePath -> IO HashList
readAndAddTree cfg mNote before path = do
  tree <- readOrBuildTree (verbose cfg) (maxdepth cfg) (exclude cfg) path
  let (NNodes n) = sumNodes tree
  log cfg $ "adding " ++ show n ++ " hashes from '" ++ path ++ "'"
  -- TODO is it weird that toSortedList includes runST?
  let res = toSortedList $ do
        after <- hashSetFromList before
        addTreeToHashSet mNote after tree
        return after
  return res

--- new version: hopefully faster and leaner ---

-- HashLine (TreeType, Depth, Hash, ModTime, NBytes, NNodes, Name)

getTreeSize :: FilePath -> IO (Maybe Int)
getTreeSize path = readLastHashLineAndFooter path >>= return . getN
  where
    getN (Just (HashLine (_,_,_,_,_, NNodes n, _), _)) = Just n
    getN Nothing = Nothing

cmdSetAdd2 :: Config -> FilePath -> Maybe String -> [FilePath] -> IO ()
cmdSetAdd2 _ _ _ [] = return () -- Docopt should prevent this, but just in case
cmdSetAdd2 cfg setPath mNoteStr treePaths = do
  let mNote = (Note . T.pack) <$> mNoteStr

  -- the actual set should be smaller (assuming some dupes),
  -- but this will prevent having to do any resizing
  maxSetSize <- (sum . catMaybes) <$> mapM getTreeSize treePaths
  log cfg $ "max expected set size: " ++ show maxSetSize

  -- exists <- SD.doesPathExist setPath
  return ()
  -- TODO make initial set the size of the (first) tree
  -- TODO read hash lines and insert them individually without building a tree
