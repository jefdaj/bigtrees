{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Directory.BigTrees.Delta
  ( Delta(..)
  , assertSameTrees
  , diff
  , findMv
  , fixMoves
  , prettyDelta
  , printDeltas
  , hPrintDeltas
  , writeDeltas
  , simDelta
  , simDeltas
  )
  where

import Control.Monad (foldM, unless)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Char8 as B8
import Data.List (find)
import Data.Maybe (fromJust)
import System.Directory.BigTrees.HashTree (HashTree (..), NodeData (..), ProdTree, addSubTree,
                                           dropTo, rmSubTree, treeName, treeType, treeHash)
import System.Directory.BigTrees.Logging (LogCfg (..), addLogContext, die, logUnsafe, LogLevel(..))
import System.Directory.BigTrees.Name (Name (..), op2ns)
import qualified System.OsPath as SOP
import System.OsPath (OsPath, decodeFS, (</>))
import System.IO (Handle, IOMode(..))
import qualified System.File.OsPath as SFO

-- TODO should these have embedded hashtrees? seems unneccesary but needed for findMoves
--      maybe only some of them are needed: add and edit. and edit only needs one.
-- TODO what should it count as when you swap L <--> B?
-- TODO do Annex and Unannex need to include the trees?
data Delta a
  = Add OsPath (HashTree a)
  | Rm OsPath
  | Mv OsPath OsPath
  | Edit    OsPath (HashTree a) (HashTree a) -- TODO remove in favor of subtle use of Add?
  | Break   OsPath (HashTree a) -- ^ Err where there was a Tree before
  | Fix     OsPath (HashTree a) -- ^ Tree where there was an Err before
  | Annex   OsPath (HashTree a) -- ^ Symlink (L/B) where there was a File before, but same hash
  | Unannex OsPath (HashTree a) -- ^ File where there was a symlink (L/B) before, but same hash
  deriving (Eq, Show)

------------------------
-- diff two hashtrees --
------------------------

-- TODO put the hashes back here?
prettyDelta :: Show a => Delta a -> IO B.ByteString
prettyDelta (Add     f _  ) = decodeFS f >>= \f' -> return $ B.pack $ "added '"     ++ f' ++ "'"
prettyDelta (Rm      f    ) = decodeFS f >>= \f' -> return $ B.pack $ "removed '"   ++ f' ++ "'"
prettyDelta (Edit    f _ _) = decodeFS f >>= \f' -> return $ B.pack $ "edited '"    ++ f' ++ "'"
prettyDelta (Break   f _  ) = decodeFS f >>= \f' -> return $ B.pack $ "broke '"     ++ f' ++ "'"
prettyDelta (Fix     f _  ) = decodeFS f >>= \f' -> return $ B.pack $ "fixed '"     ++ f' ++ "'"
prettyDelta (Annex   f _  ) = decodeFS f >>= \f' -> return $ B.pack $ "annexed '"   ++ f' ++ "'"
prettyDelta (Unannex f _  ) = decodeFS f >>= \f' -> return $ B.pack $ "unannexed '" ++ f' ++ "'"
prettyDelta (Mv f1 f2) = do
  f1' <- decodeFS f1
  f2' <- decodeFS f2
  return $ B.pack $ "moved '"   ++ f1' ++ "' -> '" ++ f2' ++ "'"

hPrintDeltas :: Show a => Handle -> [Delta a] -> IO ()
hPrintDeltas hdl ds = mapM prettyDelta ds >>= mapM_ (B.hPutStrLn hdl)

writeDeltas :: Show a => OsPath -> [Delta a] -> IO ()
writeDeltas osp ds = SFO.withBinaryFile osp WriteMode $ \hdl -> mapM prettyDelta ds >>= mapM_ (B.hPutStrLn hdl)

printDeltas :: Show a => [Delta a] -> IO ()
printDeltas ds = mapM prettyDelta ds >>= mapM_ B.putStrLn

diff :: (Eq a, Show a) => LogCfg -> HashTree a -> HashTree a -> [Delta a]
diff lCfg = diff' (addLogContext lCfg "diff") mempty

-- TODO fix non-exhaustive patterns
diff' :: (Eq a, Show a) => LogCfg -> OsPath -> HashTree a -> HashTree a -> [Delta a]

-- Break and Fix
diff' _ anchor e@(Err {}) t2 = [Fix   (anchor </> unName (treeName t2)) t2]
diff' _ anchor t1 e@(Err {}) = [Break (anchor </> unName (treeName t1)) t1]

-- Two Files
diff' lCfg anchor t1@(File {nodeData=(NodeData {name=Name f1, hash=h1})}) t2@(File {nodeData=(NodeData{name=Name f2, hash=h2})})
  | f1 == f2 && h1 == h2 = []
  | f1 /= f2 && h1 == h2 = [Mv (anchor </> f1) (anchor </> f2)]
  | f1 == f2 && h1 /= h2 = [Edit (if anchor == f1 then f1 else anchor </> f1) t1 t2]
  | otherwise = die (addLogContext lCfg "diff'") $ B8.pack $ show t1 ++ " " ++ show t2

-- File <--> Dir
-- TODO wait is this a Mv?
diff' _ anchor (File {}) t2@(Dir {nodeData=(NodeData {name=Name d})}) = [Rm anchor        , Add (anchor </> d) t2]
diff' _ anchor (Dir {nodeData=(NodeData {name=Name d})}) t2@(File {}) = [Rm (anchor </> d), Add (anchor </> d) t2]

-- Two Dirs
diff' lCfg anchor t1@(Dir {nodeData=(NodeData{hash=h1}), dirContents=os}) (Dir {nodeData=(NodeData {hash=h2}), dirContents=ns})
  -- | h1 == h2 = []
   = fixMoves lCfg t1 $ rms ++ adds ++ edits
  where
    adds  = [Add (anchor </> unName (treeName x)) x | x <- ns, treeName x `notElem` map treeName os]
    rms   = [Rm  (anchor </> unName (treeName x))   | x <- os, treeName x `notElem` map treeName ns]
    edits = concat [diff' lCfg (anchor </> unName (treeName o)) o n | o <- os, n <- ns,
                                               o /= n, treeName o == treeName n]

-- catchall
-- TODO write a b64 encoding fn so you can show the anchor outside IO?
diff' lCfg anchor t1 t2
  | treeType t1 == treeType t2 && treeHash t1 == treeHash t2 = [] -- TODO remove?
  | otherwise =
      let msg = "ERROR unexpected diff' case:'\nt1 " <> showT t1 <> "\nt2 " <> showT t2
          showT t = B8.pack $
                    "name: '"  ++ show (treeName t) ++
                    "' hash: " ++ show (treeHash t) ++
                    " type: "  ++ show (treeType t)
      in die lCfg msg

-- given two Deltas, are they a matching Rm and Add that together make a Mv?
-- TODO should also work in reverse order, right?
findMv :: (Eq a, Show a) => HashTree a -> Delta a -> Delta a -> Bool
findMv t t1@(Add _ _) t2@(Rm _) = findMv t t2 t1 -- swap order to Rm then Add
findMv t (Rm p) (Add _ t2) = case dropTo t (op2ns p) of
                               Nothing -> False
                               Just t3 -> t2 == t3
findMv _ _ _ = False

-- When a subtree with the same hash is removed and then re-added somewhere
-- else, that should be displayed as a single move operation. This will never
-- match 100% before and after actual operations, because the filesystem
-- version might be a move followed by editing files.
fixMoves :: (Eq a, Show a) => LogCfg -> HashTree a -> [Delta a] -> [Delta a]
fixMoves _ _ [] = []
fixMoves lCfg t (d1@(Rm f1):ds) = case find (findMv t d1) ds of
  Just d2@(Add f2 _) -> Mv f1 f2 : let ds' = filter (/= d2) ds in fixMoves lCfg t ds'
  Just d2            -> die (addLogContext lCfg "fixMoves") $ B8.pack $ "findMv returned a non-add: " ++ show d2
  Nothing            -> d1 : fixMoves lCfg t ds
fixMoves lCfg t (d:ds) = d : fixMoves lCfg t ds

--------------------------------------------
-- check if simulated operations are safe --
--------------------------------------------

-- TODO think through how to report results more!
-- TODO can this whole thing be trivially written in runDeltaIfSafe?

-- Can a delta be applied without losing anything?
-- TODO for efficiency, should this be part of a larger "applyIfSafe"?
--      (that would return the updated tree at the same time)
-- TODO in order to apply, need actual tree rather than just the hash!
-- safeDelta :: HashTree -> Delta -> Bool
-- safeDelta t d = safeDeltas t [d]
--
-- safeDeltas :: HashTree -> [Delta] -> Bool
-- safeDeltas t ds = case simDeltas t ds of
--   Left  _  -> False
--   Right t2 -> null $ listLostFiles t t2

-----------------------------
-- simulate git operations --
-----------------------------

-- TODO think through how to report results more!
simDelta :: (Eq a, Show a) => LogCfg -> HashTree a -> Delta a -> Either String (HashTree a)
simDelta _ t (Rm   p    ) = rmSubTree t $ op2ns p
simDelta lCfg t (Add  p   t2) = Right $ addSubTree lCfg t t2 $ op2ns p
simDelta lCfg t (Edit p _ t2) = Right $ addSubTree lCfg t t2 $ op2ns p -- TODO duplicate final name in path?
simDelta lCfg t (Mv   p1 p2) = case simDelta lCfg t (Rm p1) of
  Left  e  -> Left e
  Right t2 -> simDelta lCfg t2 $ Add p2 $ fromJust $ dropTo t $ op2ns p1 -- TODO path error here?

simDeltas :: LogCfg -> ProdTree -> [Delta ()] -> Either String ProdTree
simDeltas lCfg = foldM $ simDelta lCfg

-- seems like what we really want is runDeltaIfSafe, which does simDelta, checks safety, then runDelta

-- TODO be clearer on before/after and or expected/actual here
-- assertSameTrees :: OsPath -> HashTree -> HashTree -> IO ()
assertSameTrees :: LogCfg -> (String, ProdTree) -> (String, ProdTree) -> IO ()
assertSameTrees lCfg (msg1, tree1) (msg2, tree2) = do
  let wrong = diff lCfg tree1 tree2
  unless (null wrong) $ do
    putStrLn $ unwords ["error!", msg1, "and", msg2, "should be identical, but aren't:"]
    printDeltas wrong

-----------
-- tests --
-----------

-- I seem to have accidentally written something like an "HashTreeAction",
-- as described here:
--
-- https://jaspervdj.be/posts/2015-03-13-practical-testing-in-haskell.html
--
-- So I might as well add the rest of the scaffolding to use it in the
-- HashTree Arbitrary instance.
--
-- TODO also rename it to HashTreeAction?

-- data Delta
--   = Add  OsPath HashTree
--   | Rm   OsPath
--   | Mv   OsPath OsPath
--   | Edit OsPath HashTree -- TODO remove in favor of subtle use of Add?
--   deriving (Read, Show, Eq)

-- TODO hm, is this not the best way because of how the actions need existing trees?

-- instance Arbitrary Delta where
--   arbitrary = oneof
--     [ Add  <$> arbitrary <*> arbitrary
--     , Rm   <$> undefined -- TODO how to pick one of the existing things to delete?
--     , Mv   <$> undefined <*> undefined -- TODO how to pick an existing thing?
--     , Edit <$> undefined <*> arbitrary -- TODO same
--     ]
