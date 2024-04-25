{-# LANGUAGE QuasiQuotes #-}

module Main where

-- TODO sort by how many links deduping would save: files per dupe * (dupes - 1)
-- TODO figure out how to read files + compute hashes in parallel

import OldCmd
import Config (Config (..), defaultConfig)
import Data.Functor ((<&>))
import qualified System.Console.Docopt as D
import System.Environment (getArgs, setEnv)
import System.FilePath.Glob (compile)
import System.Locale.SetLocale

main :: IO ()
main = do

  -- TODO which is/are really needed?
  setEnv "LANG" "en_US.UTF-8"
  _ <- setLocale LC_ALL $ Just "en_US.UTF-8"

  let ptns = [D.docoptFile|app/usage.txt|]
  args <- D.parseArgsOrExit ptns =<< getArgs
  let cmd   n = D.isPresent args $ D.command n
      arg   n = D.getArgOrExitWith ptns args $ D.argument n
      lst   n = D.getAllArgs args $ D.argument n
      short n = D.getArgOrExitWith ptns args $ D.shortOption n
      flag  n = D.isPresent args $ D.shortOption n
  eList <- if flag 'e'
             then short 'e' >>= readFile <&> map compile . lines
             else return $ exclude defaultConfig
  let cfg = Config
        { bin      = D.getArg args $ D.shortOption 'b'
        , txt      = D.getArg args $ D.shortOption 't'
        , maxdepth = fmap (read :: String -> Int) $ D.getArg args $ D.shortOption 'm'
        , verbose  = flag 'v'
        , force    = flag 'f'
        , check    = flag 'c'
        , exclude  = eList
        }
  print cfg
  if cmd "cat" then do
     let paths = lst "path"
     cmdCat cfg paths
  else if cmd "hash" then do
     let paths = lst "path"
     cmdHash cfg paths
  else if cmd "diff" then do
    old <- arg "old"
    new <- arg "new"
    cmdDiff cfg old new
  else if cmd "dupes" then do
    let hashes = lst "hashes"
    cmdDupes cfg hashes
  else if cmd "test"  then do
    let paths = lst "path"
    cmdTest cfg paths
  else if cmd "update" then do
    mainTree <- arg "main"
    subTree  <- arg "sub"
    subPath  <- arg "path"
    cmdUpdate cfg mainTree subTree subPath
  -- else if cmd "rm" then do
  --   target <- arg "target"
  --   rPath  <- arg "rootpath"
  --   dPath  <- arg "rmpath"
  --   cmdRm cfg target rPath dPath
  else do
    print args
    print cfg
