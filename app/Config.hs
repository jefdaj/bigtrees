

module Config
  ( Config(..)
  , defaultConfig
  , log
  )
  where

import Control.Monad (when)
import Prelude hiding (log)
import System.OsPath (OsPath)

-- TODO derive To/FromJSON for the Config so it can go in Headers?
--      or just the exclude and maxdepth values for now

{- Parsed command line args
 - TODO add other stuff from usage.txt, or revise that
 -}
-- TODO remove from non-Cmd modules
data Config
  = Config
      { txt      :: Maybe OsPath
      , maxdepth :: Maybe Int
      , verbose  :: Bool
      -- , force    :: Bool
      , check    :: Bool
      , exclude  :: [String]
      , format   :: Maybe String
      , regex    :: Maybe String
      }
  deriving (Show)

defaultConfig :: Config
defaultConfig = Config
  { txt      = Nothing
  , maxdepth = Nothing
  , verbose  = True
  -- , force    = False
  , check    = True
  , exclude  = ["\\.sw.*", "^\\.DS_Store$", "\\.plist$", "^\\.snakemake.*"]
  , format   = Nothing
  , regex    = Nothing
  }

-- TODO remove this from Util
log :: Config -> String -> IO ()
log cfg msg = when (verbose cfg) (putStrLn msg)
