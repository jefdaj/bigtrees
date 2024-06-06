module Cmd.Info where

import System.IO -- TODO specifics
import Config (Config (..))
import Control.Exception.Safe -- TODO specifics
-- import System.Directory.BigTrees.HashTree
import System.Directory.BigTrees.Hash (Hash(..), prettyHash)
import System.Directory.BigTrees.HashLine (HashLine(..), ErrMsg(..), ModTime(..), NBytes(..), NNodes(..), parseHashLine)
import System.Directory.BigTrees.HashTree (HashTree(..), readHeader, readLastHashLineAndFooter)
import System.Directory.BigTrees.HeadFoot (Header(..), Footer, scanSeconds)
import qualified Data.ByteString.Char8 as B8
import Control.Monad (forM)
-- import qualified Data.ByteString.Short as BS

cmdInfo :: Config -> FilePath -> IO ()
cmdInfo cfg path = do
  mH  <- readHeader path
  mLF <- readLastHashLineAndFooter path
  case (mH, mLF) of
    (Just h, Just (l, f)) -> printInfo path h f l
    _ -> error $ "failed to read info from '" ++ path ++ "'"

printInfo :: FilePath -> Header -> Footer -> HashLine -> IO ()
printInfo path header footer lastLine = do
  let seconds = scanSeconds (header, footer)
      lineInfo = case lastLine of
        (ErrLine (_, ErrMsg m,_)) -> ["ERROR: " ++ m]
        (HashLine (_, _, h, ModTime m, NBytes b, NNodes n, _)) ->
          [ "contains info on " ++ show n ++ " files totaling " ++ show b ++ " bytes"
          , "overall hash is " ++ B8.unpack (prettyHash h)
          -- TODO is this accurate/useful? "modified " ++ show m
          ]
  mapM_ putStrLn $ (path ++ ":") : (map ("  " ++) $
    [ "bigtree " ++ show (treeFormat header) ++ " format"
    , "took " ++ show seconds ++ " seconds to create" -- TODO min, hours, days
    ]
    ++ lineInfo)
    -- TODO hash start date
    -- TODO n errors
