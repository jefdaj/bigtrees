module Cmd.SetAdd where

import Text.Pretty.Simple (pPrint)
import Config (Config(..))
import Control.Monad (forM)
import System.Directory.BigTrees (buildProdTree, readHashList, writeHashList, hashSetFromList, addTreeToHashSet, toSortedList, HashList)
import Control.Monad.ST (runST)
import Control.DeepSeq (force)

-- bigtrees [-v] set-add -s <set> [-n <note>] <tree>...
cmdSetAdd :: Config -> FilePath -> Maybe String -> [FilePath] -> IO ()
cmdSetAdd cfg setPath mNote treePaths = do
  -- force ensures read is strict here so it doesn't conflict with write
  eBefore <- fmap force $ readHashList setPath
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
