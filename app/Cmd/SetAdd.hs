module Cmd.SetAdd where

import Text.Pretty.Simple (pPrint)
import Config (Config(..))
import Control.Monad (forM)
import System.Directory.BigTrees (buildProdTree, readHashList, writeHashList, hashSetFromList, addTreeToHashSet, toSortedList, HashList)
import Control.Monad.ST (runST)
import Control.DeepSeq (force)
import qualified System.Directory as SD

-- bigtrees [-v] set-add -s <set> [-n <note>] <tree>...
cmdSetAdd :: Config -> FilePath -> Maybe String -> [FilePath] -> IO ()
cmdSetAdd cfg setPath mNote treePaths = do
  exists <- SD.doesPathExist setPath
  eBefore <- if exists
               -- force ensures read is strict here so it doesn't conflict with
               -- writing to the same file below
               then fmap force $ readHashList setPath
               else return $ Right []
  case eBefore of
    Left msg -> error msg
    Right before -> do
      trees  <- forM treePaths $ buildProdTree (verbose cfg) (exclude cfg)
      -- TODO is it weird that toSortedList includes runST?
      let afterL = toSortedList $ do
            after <- hashSetFromList before
            mapM_ (addTreeToHashSet after) trees
            return after
      writeHashList setPath afterL
