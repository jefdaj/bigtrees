{-# LANGUAGE QuasiQuotes #-}

module Main where

-- TODO sort by how many links deduping would save: files per dupe * (dupes - 1)
-- TODO figure out how to read files + compute hashes in parallel

import Cmd.Diff (cmdDiff)
import Cmd.Dupes (cmdDupes)
import Cmd.Find (cmdFind)
import Cmd.Hash (cmdHash)
import Cmd.Info (cmdInfo)
import Cmd.SetAdd (cmdSetAdd)
import Config (AppConfig (..), SearchConfig(..), defaultAppConfig, defaultSearchConfig)
import Data.Functor ((<&>))
import qualified System.Console.Docopt as D
import System.Environment (getArgs, setEnv)
-- import System.FilePath.Glob (compile)
import System.Locale.SetLocale (Category (LC_ALL), setLocale)
import Text.Pretty.Simple (pPrint)
import Data.Version (showVersion)
import Paths_bigtrees (version)
import System.OsPath (OsPath, encodeFS)

printVersion :: IO ()
printVersion = putStrLn $ showVersion version

main :: IO ()
main = do

  -- TODO which is/are really needed?
  setEnv "LANG" "en_US.UTF-8"
  _ <- setLocale LC_ALL $ Just "en_US.UTF-8"

  let ptns = [D.docoptFile|app/usage.txt|]
  args <- D.parseArgsOrExit ptns =<< getArgs
  pPrint args

  let cmd  n = D.isPresent  args $ D.command n
      lst  n = D.getAllArgs args $ D.argument n
      arg  n = D.getArgOrExitWith ptns args $ D.argument n
      long n = D.getArgOrExitWith ptns args $ D.longOption n
      mInt n = fmap (read :: String -> Int) $ D.getArg args $ D.longOption n
      flag n = D.isPresent args $ D.longOption n

  eList <- case D.getArg args $ D.longOption "excludes-from" of
             Nothing -> return $ excludeRegexes defaultSearchConfig
             Just f  -> readFile f <&> lines -- TODO more detailed parsing?
  sList <- case D.getArg args $ D.longOption "searches-from" of
             Nothing -> return $ searchRegexes defaultSearchConfig
             Just f  -> readFile f <&> lines -- TODO more detailed parsing?
  oPath <- case D.getArg args $ D.longOption "output" of
             Nothing -> return Nothing
             Just o  -> encodeFS o <&> Just

  let cfg = defaultAppConfig
        { outFile   = oPath
        , outFormat = D.getArg args $ D.longOption "out-fmt"
        , verbose   = flag "verbose"
        , searchCfg = defaultSearchConfig
          { minBytes   = mInt "min-size"
          , maxBytes   = mInt "max-size"
          , minDepth   = mInt "min-depth"
          , maxDepth   = mInt "max-depth"
          , minFiles   = mInt "min-files"
          , maxFiles   = mInt "max-files"
          , minModtime = mInt "min-modtime"
          , maxModtime = mInt "max-modtime"
          , excludeRegexes = eList
          , searchRegexes  = sList
          }
        }
  pPrint cfg

  if cmd "diff" then do
    old <- encodeFS =<< arg "OLD"
    new <- encodeFS =<< arg "NEW"
    cmdDiff cfg old new

  else if cmd "dupes" then do
    hashes <- encodeFS =<< arg "HASHES"
    cmdDupes cfg hashes

  else if cmd "set-add" then do
    set  <- encodeFS =<< arg "set"
    let note = D.getArg args $ D.longOption "note"
    paths <- mapM encodeFS $ lst "PATH"
    cmdSetAdd cfg set note paths

  else if cmd "find" then do
    path <- encodeFS =<< arg "PATH" -- TODO multiple paths?
    cmdFind cfg path

  else if cmd "hash" then do
     path <- encodeFS =<< arg "PATH"
     cmdHash cfg path

  else if cmd "info" then do
    path <- encodeFS =<< arg "PATH"
    cmdInfo cfg path

  else if cmd "version" then
    printVersion

  -- TODO actual exception here?
  else do
    putStrLn "no valid command given :("
