{-# LANGUAGE QuasiQuotes #-}

module Main where

-- TODO sort by how many links deduping would save: files per dupe * (dupes - 1)
-- TODO figure out how to read files + compute hashes in parallel

import Cmd.Diff (cmdDiff)
import Cmd.Dupes (cmdDupes)
import Cmd.Find (cmdFind)
import Cmd.Hash (cmdHash)
import Cmd.Info (cmdInfo)
import Config (Config (..), defaultConfig)
import Data.Functor ((<&>))
import OldCmd.Cat (oldCmdCat)
-- import OldCmd.Test (oldCmdTest)
import OldCmd.Update (oldCmdUpdate)
import qualified System.Console.Docopt as D
import System.Environment (getArgs, setEnv)
-- import System.FilePath.Glob (compile)
import System.Locale.SetLocale (Category (LC_ALL), setLocale)
-- import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do

  -- TODO which is/are really needed?
  setEnv "LANG" "en_US.UTF-8"
  _ <- setLocale LC_ALL $ Just "en_US.UTF-8"

  let ptns = [D.docoptFile|app/usage.txt|]
  args <- D.parseArgsOrExit ptns =<< getArgs
  let cmd   n = D.isPresent args $ D.command n
      arg   n = D.getArgOrExitWith ptns args $ D.argument n
      -- lst   n = D.getAllArgs args $ D.argument n
      short n = D.getArgOrExitWith ptns args $ D.shortOption n
      flag  n = D.isPresent args $ D.shortOption n
  eList <- if flag 'e'
             then short 'e' >>= readFile <&> lines
             else return $ exclude defaultConfig
  let cfg = Config
        { txt      = D.getArg args $ D.shortOption 't'
        , maxdepth = fmap (read :: String -> Int) $ D.getArg args $ D.shortOption 'd'
        , verbose  = flag 'v'
        -- , force    = flag 'f'
        , check    = flag 'c'
        , exclude  = eList
        , metafmt  = D.getArg args $ D.shortOption 'm'
        , regex    = D.getArg args $ D.shortOption 'r'
        }

  -- pPrint cfg
  -- pPrint args

  if cmd "diff" then do
    old <- arg "old"
    new <- arg "new"
    cmdDiff cfg old new

  else if cmd "dupes" then do
    hashes <- arg "hashes"
    cmdDupes cfg hashes

  else if cmd "find" then do
    path <- arg "path"
    cmdFind cfg path

  else if cmd "hash" then do
     path <- arg "path"
     cmdHash cfg path

  else if cmd "info" then do
    path <- arg "path"
    cmdInfo cfg path

  else if cmd "oldcat" then do
     path <- arg "path"
     oldCmdCat cfg path

  -- else if cmd "oldtest"  then do
  --   let paths = arg "path"
  --   oldCmdTest cfg paths

  else if cmd "oldupdate" then do
    mainTree <- arg "main"
    subTree  <- arg "sub"
    subPath  <- arg "path"
    oldCmdUpdate cfg mainTree subTree subPath

  -- TODO actual exception here?
  else do
    putStrLn "no valid command given :("
