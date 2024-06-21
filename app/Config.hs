-- TODO name the module AppConfig?
module Config
  ( AppConfig(..)
  , defaultAppConfig
  , SearchConfig(..)
  , SearchLabel
  , SearchString
  , LabeledSearchStrings
  , parseLabeledSearchStrings
  , defaultSearchConfig
  , log
  )
  where

import Control.Monad (when)
import Prelude hiding (log)
import System.OsPath (OsPath)
import System.Directory.BigTrees (SearchConfig(..), defaultSearchConfig, SearchLabel, SearchString, LabeledSearchStrings, parseLabeledSearchStrings)

-- TODO derive To/FromJSON for the AppConfig so it can go in Headers?
--      or just the exclude and maxdepth values for now

{- Parsed command line args
 - TODO add other stuff from usage.txt, or revise that
 -}
-- TODO remove from non-Cmd modules
data AppConfig = AppConfig
  { outFile   :: Maybe OsPath
  , outFormat :: Maybe String
  , searchCfg :: SearchConfig
  , verbose   :: Bool
  -- , check    :: Bool
  -- , exclude  :: [String]
  -- , force    :: Bool
  -- , maxdepth :: Maybe Int
  -- , regex    :: Maybe String
  }
  deriving (Show, Eq, Ord)

defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig
  { outFile   = Nothing
  , outFormat = Nothing
  , searchCfg = defaultSearchConfig
  , verbose   = True
  -- , check    = True
  -- , exclude  = ["\\.sw.*", "^\\.DS_Store$", "\\.plist$", "^\\.snakemake.*"]
  -- , force    = False
  -- , maxdepth = Nothing
  -- , regex    = Nothing
  }

-- TODO remove this from Util
log :: AppConfig -> String -> IO ()
log cfg msg = when (verbose cfg) (putStrLn msg)
