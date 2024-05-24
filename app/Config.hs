module Config
  ( Config(..)
  , defaultConfig
  , log
  )
  where

import Control.Monad (when)
import Prelude hiding (log)
import System.FilePath.Glob (Pattern, compile)

{- Parsed command line args
 - TODO add other stuff from usage.txt, or revise that
 -}
-- TODO remove from non-Cmd modules
data Config
  = Config
      { txt      :: Maybe FilePath
      , maxdepth :: Maybe Int
      , verbose  :: Bool
      , force    :: Bool
      , check    :: Bool
      , exclude  :: [Pattern]
      , metafmt  :: Maybe String
      , regex    :: Maybe String
      }
  deriving (Read, Show)

defaultConfig :: Config
defaultConfig = Config
  { txt      = Nothing
  , maxdepth = Nothing
  , verbose  = True
  , force    = False
  , check    = True
  , exclude  = map compile ["hashes.*", ".git*", ".*.sw*", "._DS_Store", "*.plist"]
  , metafmt  = Nothing
  , regex    = Nothing
  }

-- TODO remove this from Util
log :: Config -> String -> IO ()
log cfg msg = when (verbose cfg) (putStrLn msg)
