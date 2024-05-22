{-# LANGUAGE QuasiQuotes #-}

module Main where

-- TODO sort by how many links deduping would save: files per dupe * (dupes - 1)
-- TODO figure out how to read files + compute hashes in parallel

import Cmd.Info (cmdInfo)
import Cmd.Find (cmdFind)
import Config (Config (..), defaultConfig)
import Data.Functor ((<&>))
import OldCmd.Cat (oldCmdCat)
import OldCmd.Diff (oldCmdDiff)
import OldCmd.Dupes (oldCmdDupes)
import OldCmd.Hash (oldCmdHash)
-- import OldCmd.Test (oldCmdTest)
import OldCmd.Update (oldCmdUpdate)
import qualified System.Console.Docopt as D
import System.Environment (getArgs, setEnv)
import System.FilePath.Glob (compile)
import System.Locale.SetLocale (Category (LC_ALL), setLocale)

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

  if cmd "info" then do
    let paths = lst "path"
    cmdInfo cfg paths

  else if cmd "find" then do
    let paths = lst "path"
    cmdFind cfg paths

  else if cmd "oldcat" then do
     let paths = lst "path"
     oldCmdCat cfg paths

  else if cmd "oldhash" then do
     let paths = lst "path"
     oldCmdHash cfg paths

  else if cmd "olddiff" then do
    old <- arg "old"
    new <- arg "new"
    oldCmdDiff cfg old new

  else if cmd "olddupes" then do
    let hashes = lst "hashes"
    oldCmdDupes cfg hashes

  -- else if cmd "oldtest"  then do
  --   let paths = lst "path"
  --   oldCmdTest cfg paths

  else if cmd "oldupdate" then do
    mainTree <- arg "main"
    subTree  <- arg "sub"
    subPath  <- arg "path"
    oldCmdUpdate cfg mainTree subTree subPath

  -- TODO actual exception here?
  else do
    putStrLn "no valid command given :("
