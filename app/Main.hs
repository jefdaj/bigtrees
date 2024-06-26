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
               parseLabeledSearches, log)
import Prelude hiding (log)
import Data.Functor ((<&>))
import qualified System.Console.Docopt as D
import System.Directory.BigTrees (Depth (..), ModTime (..), NBytes (..), NNodes (..), Search (..),
                                  TreeType (..))
import System.Environment (getArgs, setEnv)
-- import System.FilePath.Glob (compile)
import Data.Maybe (fromJust)
import Data.Version (showVersion)
import Paths_bigtrees (version)
import System.Locale.SetLocale (Category (LC_ALL), setLocale)
import System.OsPath (OsPath, encodeFS)
-- import Text.Pretty.Simple (pPrint)

printVersion :: IO ()
printVersion = putStrLn $ showVersion version

main :: IO ()
main = do

  -- TODO which is/are really needed?
  setEnv "LANG" "en_US.UTF-8"
  _ <- setLocale LC_ALL $ Just "en_US.UTF-8"

  let ptns = [D.docoptFile|app/usage.txt|]
  args <- D.parseArgsOrExit ptns =<< getArgs
  -- pPrint args

  let cmd    n = D.isPresent  args $ D.command n
      flag   n = D.isPresent  args $ D.longOption n
      lstArg n = D.getAllArgs args $ D.argument n
      reqArg n = D.getArgOrExitWith ptns args $ D.longOption n
      optArg n = D.getArg args $ D.longOption n
      optRead n = read <$> optArg n

  eList <- case optArg "excludes-from" of
             Nothing -> return $ excludeRegexes defaultSearchConfig
             Just f  -> readFile f <&> lines -- TODO more detailed parsing?

  sList <- case optArg "searches-from" of

             -- get searches + labels from the file if given
             Just f -> do
               parsed <- parseLabeledSearches f
               case parsed of
                 Left  msg -> error $ show msg -- parse failure
                 Right lrs -> return lrs

             -- if no file, look for a single search + label in cli args
             Nothing -> case optArg "search-regex" of

               -- regex given; return it along with possibly-default label
               Just r -> let label = fromJust $ optArg "search-label"
                             search = Search
                                        { dirContainsPath = Nothing
                                        , baseNameMatchesRegex = Nothing
                                        , wholeNameMatchesRegex = Just r
                                        }
                         in return [(label, [search])]

               -- no regex given; use default (empty) search list
               Nothing -> return $ searches defaultSearchConfig

  oPath <- case optArg "output" of
             Nothing -> return Nothing
             Just o  -> encodeFS o <&> Just

  let cfg = defaultAppConfig
        { outFile   = oPath
        , outFormat = optArg "out-fmt"
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
          , treeTypes      = map (\c -> read [c]) <$> optArg "types"
          , excludeRegexes = eList
          , excludeSet = optArg "exclude-set"
          , searches  = sList
          }
        }

  -- pPrint cfg

  if cmd "diff" then do
    old <- encodeFS =<< reqArg "OLD"
    new <- encodeFS =<< reqArg "NEW"
    cmdDiff cfg old new

  else if cmd "dupes" then do
    hashes <- encodeFS =<< reqArg "HASHES"
    cmdDupes cfg hashes

  else if cmd "set-add" then do
    -- log cfg "set-add branch"
    set  <- encodeFS =<< reqArg "set"
    let note = optArg "note"
    paths <- mapM encodeFS $ lstArg "PATH"
    cmdSetAdd cfg set note paths

  else if cmd "find" then do
    path <- encodeFS =<< reqArg "PATH" -- TODO multiple paths?
    cmdFind cfg path

  else if cmd "hash" then do
     path <- encodeFS =<< reqArg "PATH"
     cmdHash cfg path

  else if cmd "info" then do
    path <- encodeFS =<< reqArg "PATH"
    cmdInfo cfg path

  else if cmd "version" then
    printVersion

  -- TODO actual exception here?
  else do
    putStrLn "no valid command given :("
