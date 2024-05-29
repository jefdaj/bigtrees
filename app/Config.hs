{-# LANGUAGE DeriveGeneric #-}

module Config
  ( Config(..)
  , defaultConfig
  , log
  )
  where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Control.Monad (when)
import Prelude hiding (log)

-- TODO derive To/FromJSON for the Config so it can go in Headers?
--      or just the exclude and maxdepth values for now

{- Parsed command line args
 - TODO add other stuff from usage.txt, or revise that
 -}
-- TODO remove from non-Cmd modules
data Config
  = Config
      { txt      :: Maybe FilePath
      , maxdepth :: Maybe Int
      , verbose  :: Bool
      -- , force    :: Bool
      , check    :: Bool
      , exclude  :: [String]
      , metafmt  :: Maybe String
      , regex    :: Maybe String
      }
  deriving (Read, Show, Generic)

instance NFData Config

defaultConfig :: Config
defaultConfig = Config
  { txt      = Nothing
  , maxdepth = Nothing
  , verbose  = True
  -- , force    = False
  , check    = True
  -- TODO add .nix-* things with cyclic symlinks? or fix upstream
  , exclude  = ["hashes.*", ".git*", ".*.sw*", "._DS_Store", "*.plist"]
  , metafmt  = Nothing
  , regex    = Nothing
  }

-- TODO remove this from Util
log :: Config -> String -> IO ()
log cfg msg = when (verbose cfg) (putStrLn msg)
