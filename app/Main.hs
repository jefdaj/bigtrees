{-# LANGUAGE OverloadedStrings #-}
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
import Config (AppConfig (..), SearchConfig (..), defaultAppConfig, defaultSearchConfig,
               parseLabeledSearches)
import Data.Functor ((<&>))
import Prelude hiding (log)
import qualified System.Console.Docopt as D
import System.Directory.BigTrees (Depth (..), ModTime (..), NBytes (..), NNodes (..), Search (..),
                                  TreeType (..), LogLevel(..), LogContext, log, createStderrLogger)
import System.Environment (getArgs, setEnv)
-- import System.FilePath.Glob (compile)
import Control.Monad (when)
import Data.Maybe (fromJust)
import Data.Version (showVersion)
import Paths_bigtrees (version)
import System.Locale.SetLocale (Category (LC_ALL), setLocale)
import System.OsPath (OsPath, encodeFS)
import Text.Pretty.Simple (pShow)
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as B8

printVersion :: IO ()
printVersion = putStrLn $ showVersion version

main :: IO ()
main = do

  -- TODO which is/are really needed?
  setEnv "LANG" "en_US.UTF-8"
  _ <- setLocale LC_ALL $ Just "en_US.UTF-8"

  -- TODO withTimedLogger rather than manual cleanup at the end?
  (logger, cleanupLogger) <- createStderrLogger
  let info  = log logger InfoL  "main"
      debug = log logger DebugL "main"

  debug $ B8.pack $ "bigtrees version " ++ showVersion version

  debug $ "parsing usage patterns"
  let ptns = [D.docoptFile|app/usage.txt|]
  debug $ "parsing cli args"
  args <- D.parseArgsOrExit ptns =<< getArgs

  let cmd     n = D.isPresent  args $ D.command n
      flag    n = D.isPresent  args $ D.longOption n
      lstArg  n = D.getAllArgs args $ D.argument n
      reqPathArg n = encodeFS =<< D.getArgOrExitWith ptns args (D.argument n)
      reqPathOpt n = encodeFS =<< D.getArgOrExitWith ptns args (D.longOption n)
      optLong  n = D.getArg args $ D.longOption n
      optLongs n = D.getAllArgs args $ D.longOption n
      optRead  n = read <$> optLong n

  -- can't use log here because cfg hasn't been parsed yet
  -- when (flag "verbose") $ pPrint args

  -- TODO should the main command determine which config field this goes in?
  herList <- case optLong "hash-exclude-regexes-from" of
               Nothing -> return $ hashExcludeRegexes defaultSearchConfig
               Just f  -> readFile f <&> lines -- TODO more detailed parsing?
  -- logS "herList" herList

  desList <- case optLong "dupes-exclude-searches" of

             -- get searches + labels from the file if given
             Nothing -> return []
             Just f -> do
               parsed <- parseLabeledSearches f
               case parsed of
                 Left  msg -> error $ show msg -- parse failure
                 Right lrs -> return lrs
  -- logS "desList" desList

  sList <- case optLong "searches-json" of

             -- get searches + labels from the file if given
             Just f -> do
               parsed <- parseLabeledSearches f
               case parsed of
                 Left  msg -> error $ show msg -- parse failure
                 Right lrs -> return lrs

             -- if no file, look for a single search + label in cli args
             Nothing -> case optLong "search-regex" of

               -- regex given; return it along with possibly-default label
               Just r -> let label = fromJust $ optLong "search-label"
                             search = Search
                                        { dirContainsPath = Nothing
                                        , baseNameMatchesRegex = Nothing
                                        , wholeNameMatchesRegex = Just r
                                        }
                         in return [(label, [search])]

               -- no search file given; use default (empty) search list
               Nothing -> return $ searches defaultSearchConfig
  -- logS "sList" sList

  oPath <- case optLong "output" of
             Nothing -> return Nothing
             Just o  -> encodeFS o <&> Just

  let cfg = defaultAppConfig
        { outFile   = oPath
        , findOutFormat = optLong "find-out-fmt"
        , dupesOutFormat = optLong "dupes-out-fmt"
        , verbose   = flag "verbose"
        , searchCfg = defaultSearchConfig
          { minBytes   = NBytes  <$> optRead "min-size"
          , maxBytes   = NBytes  <$> optRead "max-size"
          , minDepth   = Depth   <$> optRead "min-depth"
          , maxDepth   = Depth   <$> optRead "max-depth"
          , minFiles   = NNodes  <$> optRead "min-files"
          , maxFiles   = NNodes  <$> optRead "max-files"
          , minModtime = ModTime <$> optRead "min-modtime"
          , maxModtime = ModTime <$> optRead "max-modtime"
          , treeTypes      = map (\c -> read [c]) <$> optLong "types"
          , hashExcludeRegexes = herList
          , excludeSetPaths = optLongs "exclude-set"
          , referenceSetPaths = optLongs "reference-set"
          , searches  = sList
          , dupesExcludeSearches = desList
          }
        }

  -- logS "cfg" cfg

  -- Pass LogFn to the rest of the program, wrapped in Maybe to allow it to be
  -- disabled for tests or other contexts where someone doesn't want to bother
  -- with my logging system.
  let mLog = Just $ log logger

  if cmd "diff" then do
    debug "running diff command"
    old <- reqPathArg "OLD"
    new <- reqPathArg "NEW"
    cmdDiff cfg mLog old new

  else if cmd "dupes" then do
    debug "running dupes command"
    path <- reqPathArg "PATH"
    cmdDupes cfg mLog path

  else if cmd "set-add" then do
    debug "running set-add command"
    set  <- reqPathOpt "set"
    let note = optLong "note"
    paths <- mapM encodeFS $ lstArg "PATH"
    cmdSetAdd cfg set note paths

  else if cmd "find" then do
    debug "running find command"
    path <- reqPathArg "PATH" -- TODO multiple paths?
    cmdFind cfg mLog path

  else if cmd "hash" then do
    debug "running hash command"
    path <- reqPathArg "PATH"
    cmdHash cfg mLog path

  else if cmd "debug" then do
    -- TODO remove this command?
    debug "running debug command"
    path <- reqPathArg "PATH"
    cmdInfo cfg path

  else if cmd "version" then do
    debug "running version command"
    printVersion

  -- docopt should prevent this by aborting + printing usage
  else do
    debug "no valid command specified"
    error "probably a CLI parsing error"

  debug "cleaning up"
  cleanupLogger
